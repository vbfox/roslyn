﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.Editor.Host;
using Microsoft.CodeAnalysis.Extensions;
using Microsoft.CodeAnalysis.Shared.TestHooks;
using Microsoft.VisualStudio.Language.Intellisense;
using Microsoft.VisualStudio.Text;
using Roslyn.Utilities;

namespace Microsoft.CodeAnalysis.Editor.Implementation.Suggestions
{
    internal class SuggestedActionWithPreview : SuggestedAction, ISuggestedActionWithFlavors
    {
        private ImmutableArray<SuggestedActionSet> _actionSets;

        public SuggestedActionWithPreview(
            Workspace workspace, ITextBuffer subjectBuffer, ICodeActionEditHandlerService editHandler, 
            IWaitIndicator waitIndicator, CodeAction codeAction, object provider, 
            IAsynchronousOperationListener operationListener) 
            : base(workspace, subjectBuffer, editHandler, waitIndicator, codeAction,
                  provider, operationListener, actionSets: null)
        {
        }

        // HasActionSets is called synchronously on the UI thread. In order to avoid blocking the UI thread,
        // we need to provide a 'quick' answer here as opposed to the 'right' answer. Providing the 'right'
        // answer is expensive (because we will need to call CodeAction.GetPreviewOperationsAsync() (to
        // compute whether or not we should display the flavored action for 'Preview Changes') which in turn
        // will involve computing the changed solution for the ApplyChangesOperation for the fix / refactoring
        // So we always return 'true' here (so that platform will call GetActionSetsAsync() below). Platform
        // guarantees that nothing bad will happen if we return 'true' here and later return 'null' / empty
        // collection from within GetPreviewAsync().
        public override bool HasActionSets => true;

        public async sealed override Task<IEnumerable<SuggestedActionSet>> GetActionSetsAsync(CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            // Light bulb will always invoke this property on the UI thread.
            AssertIsForeground();

            if (_actionSets.IsDefault)
            {
                var extensionManager = this.Workspace.Services.GetService<IExtensionManager>();

                _actionSets = await extensionManager.PerformFunctionAsync(Provider, async () =>
                {
                    var builder = ArrayBuilder<SuggestedActionSet>.GetInstance();

                    // We use ConfigureAwait(true) to stay on the UI thread.
                    var previewChangesSuggestedActionSet = await GetPreviewChangesSuggestedActionSetAsync(cancellationToken).ConfigureAwait(true);
                    if (previewChangesSuggestedActionSet != null)
                    {
                        builder.Add(previewChangesSuggestedActionSet);
                    }

                    var additionalSet = this.GetAdditionalActionSet();
                    if (additionalSet != null)
                    {
                        builder.Add(additionalSet);
                    }

                    return builder.ToImmutableAndFree();
                    // We use ConfigureAwait(true) to stay on the UI thread.
                }, defaultValue: ImmutableArray<SuggestedActionSet>.Empty).ConfigureAwait(true);
            }

            Contract.ThrowIfTrue(_actionSets.IsDefault);
            return _actionSets;
        }

        protected async Task<SuggestedActionSet> GetPreviewChangesSuggestedActionSetAsync(CancellationToken cancellationToken)
        {
            var previewResult = await GetPreviewResultAsync(cancellationToken).ConfigureAwait(true);
            if (previewResult == null)
            {
                return null;
            }

            var changeSummary = previewResult.ChangeSummary;
            if (changeSummary == null)
            {
                return null;
            }

            var previewAction = new PreviewChangesCodeAction(Workspace, CodeAction, changeSummary);
            var previewSuggestedAction = new PreviewChangesSuggestedAction(
                Workspace, SubjectBuffer, EditHandler, WaitIndicator, previewAction, Provider, OperationListener);
            return new SuggestedActionSet(ImmutableArray.Create(previewSuggestedAction));
        }

        protected virtual SuggestedActionSet GetAdditionalActionSet() => null;
    }
}
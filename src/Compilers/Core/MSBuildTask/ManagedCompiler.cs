﻿// Copyright (c) Microsoft.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

using System;
using System.Collections;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Microsoft.Build.Framework;
using Microsoft.Build.Utilities;
using Microsoft.CodeAnalysis.CompilerServer;

namespace Microsoft.CodeAnalysis.BuildTasks
{
    /// <summary>
    /// This class defines all of the common stuff that is shared between the Vbc and Csc tasks.
    /// This class is not instantiatable as a Task just by itself.
    /// </summary>
    public abstract class ManagedCompiler : ToolTask
    {
        private CancellationTokenSource _sharedCompileCts = null;
        internal readonly PropertyDictionary _store = new PropertyDictionary();

        #region Properties

        // Please keep these alphabetized.
        public string[] AdditionalLibPaths
        {
            set { _store["AdditionalLibPaths"] = value; }
            get { return (string[])_store["AdditionalLibPaths"]; }
        }

        public string[] AddModules
        {
            set { _store["AddModules"] = value; }
            get { return (string[])_store["AddModules"]; }
        }

        public ITaskItem[] AdditionalFiles
        {
            set { _store["AdditionalFiles"] = value; }
            get { return (ITaskItem[])_store["AdditionalFiles"]; }
        }

        public ITaskItem[] Analyzers
        {
            set { _store["Analyzers"] = value; }
            get { return (ITaskItem[])_store["Analyzers"]; }
        }

        // We do not support BugReport because it always requires user interaction,
        // which will cause a hang.

        public string CodeAnalysisRuleSet
        {
            set { _store["CodeAnalysisRuleSet"] = value; }
            get { return (string)_store["CodeAnalysisRuleSet"]; }
        }

        public int CodePage
        {
            set { _store["CodePage"] = value; }
            get { return _store.GetOrDefault("CodePage", 0); }
        }

        public string DebugType
        {
            set { _store["DebugType"] = value; }
            get { return (string)_store["DebugType"]; }
        }

        public string DefineConstants
        {
            set { _store["DefineConstants"] = value; }
            get { return (string)_store["DefineConstants"]; }
        }

        public bool DelaySign
        {
            set { _store["DelaySign"] = value; }
            get { return _store.GetOrDefault("DelaySign", false); }
        }

        public bool EmitDebugInformation
        {
            set { _store["EmitDebugInformation"] = value; }
            get { return _store.GetOrDefault("EmitDebugInformation", false); }
        }

        public int FileAlignment
        {
            set { _store["FileAlignment"] = value; }
            get { return _store.GetOrDefault("FileAlignment", 0); }
        }

        public bool HighEntropyVA
        {
            set { _store["HighEntropyVA"] = value; }
            get { return _store.GetOrDefault("HighEntropyVA", false); }
        }

        public string KeyContainer
        {
            set { _store["KeyContainer"] = value; }
            get { return (string)_store["KeyContainer"]; }
        }

        public string KeyFile
        {
            set { _store["KeyFile"] = value; }
            get { return (string)_store["KeyFile"]; }
        }

        public ITaskItem[] LinkResources
        {
            set { _store["LinkResources"] = value; }
            get { return (ITaskItem[])_store["LinkResources"]; }
        }

        public string MainEntryPoint
        {
            set { _store["MainEntryPoint"] = value; }
            get { return (string)_store["MainEntryPoint"]; }
        }

        public bool NoConfig
        {
            set { _store["NoConfig"] = value; }
            get { return _store.GetOrDefault("NoConfig", false); }
        }

        public bool NoLogo
        {
            set { _store["NoLogo"] = value; }
            get { return _store.GetOrDefault("NoLogo", false); }
        }

        public bool NoWin32Manifest
        {
            set { _store["NoWin32Manifest"] = value; }
            get { return _store.GetOrDefault("NoWin32Manifest", false); }
        }

        public bool Optimize
        {
            set { _store["Optimize"] = value; }
            get { return _store.GetOrDefault("Optimize", false); }
        }

        [Output]
        public ITaskItem OutputAssembly
        {
            set { _store["OutputAssembly"] = value; }
            get { return (ITaskItem)_store["OutputAssembly"]; }
        }

        public string Platform
        {
            set { _store["Platform"] = value; }
            get { return (string)_store["Platform"]; }
        }

        public bool Prefer32Bit
        {
            set { _store["Prefer32Bit"] = value; }
            get { return _store.GetOrDefault("Prefer32Bit", false); }
        }

        public ITaskItem[] References
        {
            set { _store["References"] = value; }
            get { return (ITaskItem[])_store["References"]; }
        }

        public ITaskItem[] Resources
        {
            set { _store["Resources"] = value; }
            get { return (ITaskItem[])_store["Resources"]; }
        }

        public ITaskItem[] ResponseFiles
        {
            set { _store["ResponseFiles"] = value; }
            get { return (ITaskItem[])_store["ResponseFiles"]; }
        }

        public ITaskItem[] Sources
        {
            set
            {
                if (UsedCommandLineTool)
                {
                    NormalizePaths(value);
                }

                _store["Sources"] = value;
            }
            get { return (ITaskItem[])_store["Sources"]; }
        }

        public string SubsystemVersion
        {
            set { _store["SubsystemVersion"] = value; }
            get { return (string)_store["SubsystemVersion"]; }
        }

        public string TargetType
        {
            set { _store["TargetType"] = value.ToLower(CultureInfo.InvariantCulture); }
            get { return (string)_store["TargetType"]; }
        }

        public bool TreatWarningsAsErrors
        {
            set { _store["TreatWarningsAsErrors"] = value; }
            get { return _store.GetOrDefault("TreatWarningsAsErrors", false); }
        }

        public bool Utf8Output
        {
            set { _store["Utf8Output"] = value; }
            get { return _store.GetOrDefault("Utf8Output", false); }
        }

        public string Win32Icon
        {
            set { _store["Win32Icon"] = value; }
            get { return (string)_store["Win32Icon"]; }
        }

        public string Win32Manifest
        {
            set { _store["Win32Manifest"] = value; }
            get { return (string)_store["Win32Manifest"]; }
        }

        public string Win32Resource
        {
            set { _store["Win32Resource"] = value; }
            get { return (string)_store["Win32Resource"]; }
        }

        /// <summary>
        /// If this property is true then the task will take every C# or VB
        /// compilation which is queued by MSBuild and send it to the 
        /// VBCSCompiler server instance, starting a new instance if necessary.
        /// If false, we will use the values from ToolPath/Exe.
        /// </summary>
        public bool UseSharedCompilation
        {
            set { _store[nameof(UseSharedCompilation)] = value; }
            get { return _store.GetOrDefault(nameof(UseSharedCompilation), false); }
        }

        // Map explicit platform of "AnyCPU" or the default platform (null or ""), since it is commonly understood in the 
        // managed build process to be equivalent to "AnyCPU", to platform "AnyCPU32BitPreferred" if the Prefer32Bit 
        // property is set. 
        internal string PlatformWith32BitPreference
        {
            get
            {
                string platform = this.Platform;
                if ((String.IsNullOrEmpty(platform) || platform.Equals("anycpu", StringComparison.OrdinalIgnoreCase)) && this.Prefer32Bit)
                {
                    platform = "anycpu32bitpreferred";
                }
                return platform;
            }
        }

        /// <summary>
        /// Overridable property specifying the encoding of the captured task standard output stream
        /// </summary>
        protected override Encoding StandardOutputEncoding
        {
            get
            {
                return (Utf8Output) ? Encoding.UTF8 : base.StandardOutputEncoding;
            }
        }

        #endregion

        internal abstract BuildProtocolConstants.RequestLanguage Language { get; }

        protected override int ExecuteTool(string pathToTool, string responseFileCommands, string commandLineCommands)
        {
            if (!UseSharedCompilation)
            {
                return base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
            }

            using (_sharedCompileCts = new CancellationTokenSource())
            {
                try
                {
                    var responseTask = BuildClient.TryRunServerCompilation(
                        Language,
                        CurrentDirectoryToUse(),
                        GetArguments(commandLineCommands, responseFileCommands),
                        _sharedCompileCts.Token,
                        libEnvVariable: LibDirectoryToUse());

                    responseTask.Wait(_sharedCompileCts.Token);

                    ExitCode = responseTask.Result != null
                        ? HandleResponse(responseTask.Result)
                        : base.ExecuteTool(pathToTool, responseFileCommands, commandLineCommands);
                }
                catch (OperationCanceledException)
                {
                    ExitCode = 0;
                }
                catch (Exception e)
                {
                    Log.LogErrorWithCodeFromResources("Compiler.UnexpectedException");
                    LogErrorOutput(e.ToString());
                    ExitCode = -1;
                }
            }
            return ExitCode;
        }

        /// <summary>
        /// Cancel the in-process build task.
        /// </summary>
        public override void Cancel()
        {
            base.Cancel();

            _sharedCompileCts?.Cancel();
        }

        /// <summary>
        /// Get the current directory that the compiler should run in.
        /// </summary>
        private string CurrentDirectoryToUse()
        {
            // ToolTask has a method for this. But it may return null. Use the process directory
            // if ToolTask didn't override. MSBuild uses the process directory.
            string workingDirectory = GetWorkingDirectory();
            if (string.IsNullOrEmpty(workingDirectory))
                workingDirectory = Environment.CurrentDirectory;
            return workingDirectory;
        }

        /// <summary>
        /// Get the "LIB" environment variable, or NULL if none.
        /// </summary>
        private string LibDirectoryToUse()
        {
            // First check the real environment.
            string libDirectory = Environment.GetEnvironmentVariable("LIB");

            // Now go through additional environment variables.
            string[] additionalVariables = this.EnvironmentVariables;
            if (additionalVariables != null)
            {
                foreach (string var in this.EnvironmentVariables)
                {
                    if (var.StartsWith("LIB=", StringComparison.OrdinalIgnoreCase))
                    {
                        libDirectory = var.Substring(4);
                    }
                }
            }

            return libDirectory;
        }

        /// <summary>
        /// The return code of the compilation. Strangely, this isn't overridable from ToolTask, so we need 
        /// to create our own.
        /// </summary>
        [Output]
        public new int ExitCode { get; private set; } = 0;

        /// <summary>
        /// Handle a response from the server, reporting messages and returning
        /// the appropriate exit code.
        /// </summary>
        private int HandleResponse(BuildResponse response)
        {
            var completedResponse = response as CompletedBuildResponse;
            if (completedResponse != null)
            {
                LogMessages(completedResponse.Output, this.StandardOutputImportanceToUse);

                if (LogStandardErrorAsError)
                {
                    LogErrorOutput(completedResponse.ErrorOutput);
                }
                else
                {
                    LogMessages(completedResponse.ErrorOutput, this.StandardErrorImportanceToUse);
                }

                ExitCode = completedResponse.ReturnCode;
            }
            else
            {
                Debug.Assert(response is MismatchedVersionBuildResponse);

                LogErrorOutput("Roslyn compiler server reports different protocol version than build task.");
                ExitCode = -1;
            }
            return ExitCode;
        }

        private void LogErrorOutput(string output)
        {
            string[] lines = output.Split(new string[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries);
            foreach (string line in lines)
            {
                string trimmedMessage = line.Trim();
                if (trimmedMessage != "")
                {
                    Log.LogError(trimmedMessage);
                }
            }
        }

        /// <summary>
        /// Log each of the messages in the given output with the given importance.
        /// We assume each line is a message to log.
        /// </summary>
        /// <remarks>
        /// Should be "private protected" visibility once it is introduced into C#.
        /// </remarks>
        internal abstract void LogMessages(string output, MessageImportance messageImportance);

        /// <summary>
        /// Get the command line arguments to pass to the compiler.
        /// </summary>
        private string[] GetArguments(string commandLineCommands, string responseFileCommands)
        {
            CompilerServerLogger.Log($"CommandLine = '{commandLineCommands}'");
            CompilerServerLogger.Log($"BuildResponseFile = '{responseFileCommands}'");

            var commandLineArguments =
                CommandLineParser.SplitCommandLineIntoArguments(commandLineCommands, removeHashComments: true);
            var responseFileArguments =
                CommandLineParser.SplitCommandLineIntoArguments(responseFileCommands, removeHashComments: true);
            return commandLineArguments.Concat(responseFileArguments).ToArray();
        }

        /// <summary>
        /// Returns the command line switch used by the tool executable to specify the response file
        /// Will only be called if the task returned a non empty string from GetResponseFileCommands
        /// Called after ValidateParameters, SkipTaskExecution and GetResponseFileCommands
        /// </summary>
        override protected string GenerateResponseFileCommands()
        {
            CommandLineBuilderExtension commandLineBuilder = new CommandLineBuilderExtension();
            AddResponseFileCommands(commandLineBuilder);
            return commandLineBuilder.ToString();
        }

        protected override string GenerateCommandLineCommands()
        {
            CommandLineBuilderExtension commandLineBuilder = new CommandLineBuilderExtension();
            AddCommandLineCommands(commandLineBuilder);
            return commandLineBuilder.ToString();
        }

        /// <summary>
        /// Fills the provided CommandLineBuilderExtension with those switches and other information that can't go into a response file and
        /// must go directly onto the command line.
        /// </summary>
        protected internal virtual void AddCommandLineCommands(CommandLineBuilderExtension commandLine)
        {
            commandLine.AppendWhenTrue("/noconfig", this._store, "NoConfig");
        }

        /// <summary>
        /// Fills the provided CommandLineBuilderExtension with those switches and other information that can go into a response file.
        /// </summary>
        protected internal virtual void AddResponseFileCommands(CommandLineBuilderExtension commandLine)
        {
            // If outputAssembly is not specified, then an "/out: <name>" option won't be added to
            // overwrite the one resulting from the OutputAssembly member of the CompilerParameters class.
            // In that case, we should set the outputAssembly member based on the first source file.
            if (
                    (OutputAssembly == null) &&
                    (Sources != null) &&
                    (Sources.Length > 0) &&
                    (this.ResponseFiles == null)    // The response file may already have a /out: switch in it, so don't try to be smart here.
                )
            {
                try
                {
                    OutputAssembly = new TaskItem(Path.GetFileNameWithoutExtension(Sources[0].ItemSpec));
                }
                catch (ArgumentException e)
                {
                    throw new ArgumentException(e.Message, "Sources");
                }
                if (String.Compare(TargetType, "library", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    OutputAssembly.ItemSpec += ".dll";
                }
                else if (String.Compare(TargetType, "module", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    OutputAssembly.ItemSpec += ".netmodule";
                }
                else
                {
                    OutputAssembly.ItemSpec += ".exe";
                }
            }

            commandLine.AppendSwitchIfNotNull("/addmodule:", this.AddModules, ",");
            commandLine.AppendSwitchWithInteger("/codepage:", this._store, "CodePage");

            ConfigureDebugProperties();

            // The "DebugType" parameter should be processed after the "EmitDebugInformation" parameter
            // because it's more specific.  Order matters on the command-line, and the last one wins.
            // /debug+ is just a shorthand for /debug:full.  And /debug- is just a shorthand for /debug:none.

            commandLine.AppendPlusOrMinusSwitch("/debug", this._store, "EmitDebugInformation");
            commandLine.AppendSwitchIfNotNull("/debug:", this.DebugType);

            commandLine.AppendPlusOrMinusSwitch("/delaysign", this._store, "DelaySign");

            commandLine.AppendSwitchWithInteger("/filealign:", this._store, "FileAlignment");
            commandLine.AppendSwitchIfNotNull("/keycontainer:", this.KeyContainer);
            commandLine.AppendSwitchIfNotNull("/keyfile:", this.KeyFile);
            // If the strings "LogicalName" or "Access" ever change, make sure to search/replace everywhere in vsproject.
            commandLine.AppendSwitchIfNotNull("/linkresource:", this.LinkResources, new string[] { "LogicalName", "Access" });
            commandLine.AppendWhenTrue("/nologo", this._store, "NoLogo");
            commandLine.AppendWhenTrue("/nowin32manifest", this._store, "NoWin32Manifest");
            commandLine.AppendPlusOrMinusSwitch("/optimize", this._store, "Optimize");
            commandLine.AppendSwitchIfNotNull("/out:", this.OutputAssembly);
            commandLine.AppendSwitchIfNotNull("/ruleset:", this.CodeAnalysisRuleSet);
            commandLine.AppendSwitchIfNotNull("/subsystemversion:", this.SubsystemVersion);
            // If the strings "LogicalName" or "Access" ever change, make sure to search/replace everywhere in vsproject.
            commandLine.AppendSwitchIfNotNull("/resource:", this.Resources, new string[] { "LogicalName", "Access" });
            commandLine.AppendSwitchIfNotNull("/target:", this.TargetType);
            commandLine.AppendPlusOrMinusSwitch("/warnaserror", this._store, "TreatWarningsAsErrors");
            commandLine.AppendWhenTrue("/utf8output", this._store, "Utf8Output");
            commandLine.AppendSwitchIfNotNull("/win32icon:", this.Win32Icon);
            commandLine.AppendSwitchIfNotNull("/win32manifest:", this.Win32Manifest);

            // Append the analyzers.
            this.AddAnalyzersToCommandLine(commandLine);

            // Append additional files.
            this.AddAdditionalFilesToCommandLine(commandLine);

            // Append the sources.
            commandLine.AppendFileNamesIfNotNull(Sources, " ");
        }

        /// <summary>
        /// Adds a "/analyzer:" switch to the command line for each provided analyzer.
        /// </summary>
        private void AddAnalyzersToCommandLine(CommandLineBuilderExtension commandLine)
        {
            // If there were no analyzers passed in, don't add any /analyzer: switches
            // on the command-line.
            if ((this.Analyzers == null) || (this.Analyzers.Length == 0))
            {
                return;
            }

            foreach (ITaskItem analyzer in this.Analyzers)
            {
                commandLine.AppendSwitchIfNotNull("/analyzer:", analyzer.ItemSpec);
            }
        }

        /// <summary>
        /// Adds a "/analyzer:" switch to the command line for each provided analyzer.
        /// </summary>
        private void AddAdditionalFilesToCommandLine(CommandLineBuilderExtension commandLine)
        {
            // If there were no additional files passed in, don't add any /additionalfile: switches
            // on the command-line.
            if ((this.AdditionalFiles == null) || (this.AdditionalFiles.Length == 0))
            {
                return;
            }

            foreach (ITaskItem additionalFile in this.AdditionalFiles)
            {
                commandLine.AppendSwitchIfNotNull("/additionalfile:", additionalFile.ItemSpec);
            }
        }

        /// <summary>
        /// Configure the debug switches which will be placed on the compiler commandline.
        /// The matrix of debug type and symbol inputs and the desired results is as follows:
        /// 
        /// Debug Symbols              DebugType   Desired Resilts
        ///          True               Full        /debug+ /debug:full
        ///          True               PdbOnly     /debug+ /debug:PdbOnly
        ///          True               None        /debug-
        ///          True               Blank       /debug+
        ///          False              Full        /debug- /debug:full
        ///          False              PdbOnly     /debug- /debug:PdbOnly
        ///          False              None        /debug-
        ///          False              Blank       /debug-
        ///          Blank              Full                /debug:full
        ///          Blank              PdbOnly             /debug:PdbOnly
        ///          Blank              None        /debug-
        /// Debug:   Blank              Blank       /debug+ //Microsof.common.targets will set this
        /// Release: Blank              Blank       "Nothing for either switch"
        /// 
        /// The logic is as follows:
        /// If debugtype is none  set debugtype to empty and debugSymbols to false
        /// If debugType is blank  use the debugsymbols "as is"
        /// If debug type is set, use its value and the debugsymbols value "as is"
        /// </summary>
        private void ConfigureDebugProperties()
        {
            // If debug type is set we need to take some action depending on the value. If debugtype is not set
            // We don't need to modify the EmitDebugInformation switch as its value will be used as is.
            if (_store["DebugType"] != null)
            {
                // If debugtype is none then only show debug- else use the debug type and the debugsymbols as is.
                if (string.Compare((string)_store["DebugType"], "none", StringComparison.OrdinalIgnoreCase) == 0)
                {
                    _store["DebugType"] = null;
                    _store["EmitDebugInformation"] = false;
                }
            }
        }

        /// <summary>
        /// Validate parameters, log errors and warnings and return true if
        /// Execute should proceed.
        /// </summary>
        protected override bool ValidateParameters()
        {
            return ListHasNoDuplicateItems(this.Resources, "Resources", "LogicalName") && ListHasNoDuplicateItems(this.Sources, "Sources");
        }

        /// <summary>
        /// Returns true if the provided item list contains duplicate items, false otherwise.
        /// </summary>
        protected bool ListHasNoDuplicateItems(ITaskItem[] itemList, string parameterName)
        {
            return ListHasNoDuplicateItems(itemList, parameterName, null);
        }

        /// <summary>
        /// Returns true if the provided item list contains duplicate items, false otherwise.
        /// </summary>
        /// <param name="itemList"></param>
        /// <param name="disambiguatingMetadataName">Optional name of metadata that may legitimately disambiguate items. May be null.</param>
        /// <param name="parameterName"></param>
        private bool ListHasNoDuplicateItems(ITaskItem[] itemList, string parameterName, string disambiguatingMetadataName)
        {
            if (itemList == null || itemList.Length == 0)
            {
                return true;
            }

            Hashtable alreadySeen = new Hashtable(StringComparer.OrdinalIgnoreCase);
            foreach (ITaskItem item in itemList)
            {
                string key;
                string disambiguatingMetadataValue = null;
                if (disambiguatingMetadataName != null)
                {
                    disambiguatingMetadataValue = item.GetMetadata(disambiguatingMetadataName);
                }

                if (disambiguatingMetadataName == null || String.IsNullOrEmpty(disambiguatingMetadataValue))
                {
                    key = item.ItemSpec;
                }
                else
                {
                    key = item.ItemSpec + ":" + disambiguatingMetadataValue;
                }

                if (alreadySeen.ContainsKey(key))
                {
                    if (disambiguatingMetadataName == null || String.IsNullOrEmpty(disambiguatingMetadataValue))
                    {
                        Log.LogErrorWithCodeFromResources("General.DuplicateItemsNotSupported", item.ItemSpec, parameterName);
                    }
                    else
                    {
                        Log.LogErrorWithCodeFromResources("General.DuplicateItemsNotSupportedWithMetadata", item.ItemSpec, parameterName, disambiguatingMetadataValue, disambiguatingMetadataName);
                    }
                    return false;
                }
                else
                {
                    alreadySeen[key] = String.Empty;
                }
            }

            return true;
        }

        /// <summary>
        /// Allows tool to handle the return code.
        /// This method will only be called with non-zero exitCode.
        /// </summary>
        protected override bool HandleTaskExecutionErrors()
        {
            // For managed compilers, the compiler should emit the appropriate
            // error messages before returning a non-zero exit code, so we don't 
            // normally need to emit any additional messages now.
            //
            // If somehow the compiler DID return a non-zero exit code and didn't log an error, we'd like to log that exit code.
            // We can only do this for the command line compiler: if the inproc compiler was used, 
            // we can't tell what if anything it logged as it logs directly to Visual Studio's output window.
            //
            if (!Log.HasLoggedErrors && UsedCommandLineTool)
            {
                // This will log a message "MSB3093: The command exited with code {0}."
                base.HandleTaskExecutionErrors();
            }

            return false;
        }

        /// <summary>
        /// Takes a list of files and returns the normalized locations of these files
        /// </summary>
        private void NormalizePaths(ITaskItem[] taskItems)
        {
            foreach (var item in taskItems)
            {
                item.ItemSpec = Utilities.GetFullPathNoThrow(item.ItemSpec);
            }
        }

        /// <summary>
        /// Whether the command line compiler was invoked, instead
        /// of the host object compiler.
        /// </summary>
        protected bool UsedCommandLineTool
        {
            get;
            set;
        }

        private bool _hostCompilerSupportsAllParameters;
        protected bool HostCompilerSupportsAllParameters
        {
            get { return _hostCompilerSupportsAllParameters; }
            set { _hostCompilerSupportsAllParameters = value; }
        }

        /// <summary>
        /// Checks the bool result from calling one of the methods on the host compiler object to
        /// set one of the parameters.  If it returned false, that means the host object doesn't
        /// support a particular parameter or variation on a parameter.  So we log a comment,
        /// and set our state so we know not to call the host object to do the actual compilation.
        /// </summary>
        /// <owner>RGoel</owner>
        protected void CheckHostObjectSupport
            (
            string parameterName,
            bool resultFromHostObjectSetOperation
            )
        {
            if (!resultFromHostObjectSetOperation)
            {
                Log.LogMessageFromResources(MessageImportance.Normal, "General.ParameterUnsupportedOnHostCompiler", parameterName);
                _hostCompilerSupportsAllParameters = false;
            }
        }

        /// <summary>
        /// Checks to see whether all of the passed-in references exist on disk before we launch the compiler.
        /// </summary>
        /// <owner>RGoel</owner>
        protected bool CheckAllReferencesExistOnDisk()
        {
            if (null == this.References)
            {
                // No references
                return true;
            }

            bool success = true;

            foreach (ITaskItem reference in this.References)
            {
                if (!File.Exists(reference.ItemSpec))
                {
                    success = false;
                    Log.LogErrorWithCodeFromResources("General.ReferenceDoesNotExist", reference.ItemSpec);
                }
            }

            return success;
        }

        /// <summary>
        /// The IDE and command line compilers unfortunately differ in how win32 
        /// manifests are specified.  In particular, the command line compiler offers a 
        /// "/nowin32manifest" switch, while the IDE compiler does not offer analagous 
        /// functionality. If this switch is omitted from the command line and no win32 
        /// manifest is specified, the compiler will include a default win32 manifest 
        /// named "default.win32manifest" found in the same directory as the compiler 
        /// executable. Again, the IDE compiler does not offer analagous support.
        /// 
        /// We'd like to imitate the command line compiler's behavior in the IDE, but 
        /// it isn't aware of the default file, so we must compute the path to it if 
        /// noDefaultWin32Manifest is false and no win32Manifest was provided by the
        /// project.
        ///
        /// This method will only be called during the initialization of the host object,
        /// which is only used during IDE builds.
        /// </summary>
        /// <returns>the path to the win32 manifest to provide to the host object</returns>
        internal string GetWin32ManifestSwitch
        (
            bool noDefaultWin32Manifest,
            string win32Manifest
        )
        {
            if (!noDefaultWin32Manifest)
            {
                if (String.IsNullOrEmpty(win32Manifest) && String.IsNullOrEmpty(this.Win32Resource))
                {
                    // We only want to consider the default.win32manifest if this is an executable
                    if (!String.Equals(TargetType, "library", StringComparison.OrdinalIgnoreCase)
                       && !String.Equals(TargetType, "module", StringComparison.OrdinalIgnoreCase))
                    {
                        // We need to compute the path to the default win32 manifest
                        string pathToDefaultManifest = ToolLocationHelper.GetPathToDotNetFrameworkFile
                                                       (
                                                           "default.win32manifest",
                                                           TargetDotNetFrameworkVersion.VersionLatest
                                                       );

                        if (null == pathToDefaultManifest)
                        {
                            // This is rather unlikely, and the inproc compiler seems to log an error anyway.
                            // So just a message is fine.
                            Log.LogMessageFromResources
                            (
                                "General.ExpectedFileMissing",
                                "default.win32manifest"
                            );
                        }

                        return pathToDefaultManifest;
                    }
                }
            }

            return win32Manifest;
        }
    }
}
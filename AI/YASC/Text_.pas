unit Text_; {Kludge: 'SokUtil_' also contains some texts because the kernel is self-contained}


interface

uses Windows,MPlayer,Classes,Misc_,Game_,MPlayer1_,BitMap_,SokUtil_,SokFile_;

// Most of the texts may be localized, but formatting tags (e.g., %d, %f, and %s) must always be left unchanged
const
  ACCEL_CHAR_ADD                 = Chr(VK_F4);        // add tasks to solver task queue and optimizer task queue
  ACCEL_CHAR_BOOKMARKS           = Chr(VK_RETURN);    // save/open bookmark (not in production anymore; superseded by the snapshot feature)
  ACCEL_CHAR_CAPTURE_SCREEN      = 'K';               // capture screen
  ACCEL_CHAR_CLOSE               = 'C';               // close
  ACCEL_CHAR_CLOSE_MPLAYER_FORM  = 'L';               // close 'MPlayerForm'
  ACCEL_CHAR_COPY_BOARD_TO_CLIPBOARD
                                 = 'B';               // copy board to clipboard, as opposed to copying the entire level to the clipboard
  ACCEL_CHAR_COPY_SOLUTION_MOVES_TO_CLIPBOARD
                                 = 'M';               // copy best solution/moves to clipboard
  ACCEL_CHAR_COPY_SOLUTION_PUSHES_TO_CLIPBOARD
                                 = 'P';               // copy best solution/pushes to clipboard
  ACCEL_CHAR_COPY_TO_CLIPBOARD   = 'C';               // copy to clipboard
  ACCEL_CHAR_COPY_TO_CLIPBOARD_FORMAT
                                 = 'F';               // copy to clipboard format
  ACCEL_CHAR_COPY_CONTINUATION_MOVES_TO_CLIPBOARD
                                 = 'G';
  ACCEL_CHAR_COPY_COLLECTION_TO_CLIPBOARD             // copy collection to clipboard
                                 = 'K';
  ACCEL_CHAR_COPY_COLLECTION_SOLUTIONS_TO_CLIPBOARD   // copy collection solutions to clipboard
                                 = 'G';
  ACCEL_CHAR_COPY_MOVES_TO_CLIPBOARD
                                 = 'F';               // copy moves (current game) to clipboard
  ACCEL_CHAR_CUT_TO_CLIPBOARD    = 'X';               // cut to clipboard
  ACCEL_CHAR_DELETE              = 'D';               // delete
  ACCEL_CHAR_DUPLICATE_LEVELS_CURRENT_LEVEL           // find levels similar to current level (must match shortcut for menu item on 'TOpenForm')
                                 = Chr(VK_F4);        // note that it must be combined with [CTRL]
  ACCEL_CHAR_DUPLICATE_LEVELS_CURRENT_COLLECTION      // find duplicates of the current collection (must match shortcut for menu item on 'TOpenForm')
                                 = Chr(VK_F5);        // note that it must be combined with [CTRL]
  ACCEL_CHAR_DUPLICATE_LEVELS_ALL_LEVELS              // find all duplicate levels (must match shortcut for menu item on 'TOpenForm')
                                 = Chr(VK_F8);        // note that it must be combined with [CTRL]
  ACCEL_CHAR_EDIT                = 'D';               // edit
  ACCEL_CHAR_EXIT                = 'X';               // exit
  ACCEL_CHAR_GAME_MODE           = 'R';               // toggle game mode (normal/reverse)
  ACCEL_CHAR_GAME_MODE_NORMAL    = 'N';               // normal game mode
  ACCEL_CHAR_HELP                = 'H';               // help
  ACCEL_CHAR_IMAGE_TEXT          = Chr(VK_F2);        // note that it must be combined with [CTRL]
  ACCEL_CHAR_LOWER_BOUND         = '0';               // calculate lower bound, pushes
  ACCEL_CHAR_MENU                = 'U';               // menu; note: the popup menu cannot have an item with the same accelerator because it would fire immidiately
  ACCEL_CHAR_NEW                 = 'N';               // new
  ACCEL_CHAR_OPEN_PRIOR          = Chr(VK_PRIOR);     // open prior
  ACCEL_CHAR_OPEN                = 'O';               // open
  ACCEL_CHAR_OPEN1               = 'O';               // open1
  ACCEL_CHAR_OPEN_NEXT           = Chr(VK_NEXT);      // open next
  ACCEL_CHAR_OPTIONS             = 'E';               // options
  ACCEL_CHAR_PASTE_FROM_CLIPBOARD= 'V';               // paste from clipboard
  ACCEL_CHAR_PAUSE               = 'P';               // pause (musicplayer and toolsform replay)
  ACCEL_CHAR_PLAY                = 'P';               // play  (musicplayer and toolsform replay)
  ACCEL_CHAR_REDO                = Chr(VK_INSERT);    // redo  (caution: changing this shortcut requires text changes as well, e.g., 'HintUse_INSERT_And_DELETE_ToStepText')
  ACCEL_CHAR_REDO_ALL            = Chr(VK_END);       // redo all moves
  ACCEL_CHAR_REDO_EDIT           = 'Y';               // redo text edit
  ACCEL_CHAR_RELEASE_NOTES       = 'R';               // release notes (help form)
  ACCEL_CHAR_REORGANIZE_FILE     = 'R';               // reorganize file (open form)
  ACCEL_CHAR_REPLAY              = 'P';               // replay
  ACCEL_CHAR_RENAME              = 'R';               // rename
  ACCEL_CHAR_RESET               = 'R';               // reset
  ACCEL_CHAR_OPEN_SNAPSHOTS      = ']';               // open snapshots window
  ACCEL_CHAR_SOLVE               = 'V';               // start solver
  ACCEL_CHAR_SPLIT_VIEW          = 'W';               // split view
  ACCEL_CHAR_STOP                = 'S';               // stop (musicplayer and toolsform replay)
  ACCEL_CHAR_RESTART_GAME        = Chr(VK_HOME);      // restart game
  ACCEL_CHAR_ROTATE_FLIP_BOARD_1 = 'J';               // rotate and flip board
  ACCEL_CHAR_ROTATE_FLIP_BOARD_2 = 'K';               // rotate and flip board
  ACCEL_CHAR_ROTATE_MIRROR_BOARD_RESET
                                 = 'Y';               // reset rotate and mirror board
  ACCEL_CHAR_SAVE                = 'S';               // save
  ACCEL_CHAR_SAVE_AS             = 'A';               // save as
  ACCEL_CHAR_SCREENSHOT          = Chr(VK_OEM_PERIOD);// make screenshot
  ACCEL_CHAR_SELECT_ALL          = 'A';               // select all
  ACCEL_CHAR_SOLUTION            = 'L';               // show solution
  ACCEL_CHAR_SOLUTION_MOVES      = 'M';               // show solution/moves
  ACCEL_CHAR_SOLUTION_PUSHES     = 'P';               // show solution/pushes
  ACCEL_CHAR_SOLVER              = 'Q';               // open solver
  ACCEL_CHAR_STOP_CALCULATING_DEADLOCKS
                                 = 'I';               // stop calculating more deadlock situations
  ACCEL_CHAR_TOOLS               = 'T';               // tools
  ACCEL_CHAR_UNDO                = Chr(VK_DELETE);    // undo  (caution: changing this shortcut requires text changes as well, e.g., 'HintUse_INSERT_And_DELETE_ToStepText')
  ACCEL_CHAR_UNDO1               = Chr(VK_BACK);      // undo1 (caution: changing this shortcut requires text changes as well, e.g., 'HintUse_INSERT_And_DELETE_ToStepText')
  ACCEL_CHAR_UNDO_EDIT           = 'Z';               // undo text edit
  ACCEL_CHAR_SNAPSHOTS           = Chr(VK_RETURN);    // save/open snapshot

  BTN_HELP_CAPTION               = '&Help';
  BTN_EXIT_CAPTION               = 'E&xit';
  BTN_MODE_CAPTION               : array[Boolean] of String
                                 = ('Normal','Reverse');
  BTN_OPEN_CAPTION               = '&Open...';
  BTN_OPTIONS_CAPTION            = 'S&ettings';
  BTN_SAVE_CAPTION               = '&Save';
  BTN_SAVE_AS_CAPTION            = '&as...';
  BTN_SOLUTION_CAPTION           = 'So&lution';
  BTN_TOOLS_CAPTION              = '&Tools';

  ApplicationName                = 'Sokoban YASC';
  DoYouWantToRefreshTheDuplicateLevelsWindowText
                                 = 'Please note that the "Duplicate Levels" window contents are outdated now.'+NL+NL+
                                   'Do you want to refresh the view by rerunning the scan?';

  DoYouWantToExitTheProgramText  = 'Do you want to exit the program?';
  DoYouWantToSaveItText          = 'Do you want to save it?';
  StopText                       = 'Stop';
  TextFilesFilterText            = 'Text-files  (*.txt)|*.txt';

  ActivitiesMenuText             : array[0..4] of String = (
                                   'Activities','Mandala','Images',{'Fireworks',}'Fractals','Settings');
  ActivityText                   : array[tActivity] of String = (
                                     'None','Mandala','Images',{'Fireworks',}'Fractals');
  AddFolderCaptionText           = 'Add Folder';
  AllFilesText                   = 'All files';
  AllFilesFilterText             = AllFilesText+'  ('+ALL_FILES_FILTER+')|'+ALL_FILES_FILTER;
  AllTransactionsText            = 'All transactions';
  AllTransactionsText__          = 'All %d transactions';
  AlternatingOptimizationsRepeatTypeText
                                 : array[Boolean] of String = (
                                   'Repeat if any metrics improve',
                                   'Repeat if moves, pushes, or box lines improve');
  AlternatingOptimizationsTriggerTypeText
                                 : array[Boolean] of String = (
                                   'At least once',
                                   'Only after finding improvements');
  AlternatingOptimizationsText   : array[TAlternatingOptimizations] of String = (
                                     'Moves/pushes',
                                     'Box lines/primary',
                                     'Moves/pushes, then box lines',
                                     'Box lines, then moves/pushes');
  AntiAliasingText               : array[TAntiAliasing] of String = ( // changed to these more informative names in version 1.647. the earlier versions are 'Antialiasing2Text' and 'Antialiasing3Text'
                                     'None','Low quality','High quality');
  Antialiasing2Text              : array[TAntialiasing] of String = ( // texts used before version 1.233
                                     'None','Bilinear','Sine filter');
  AntiAliasing3Text              : array[TAntiAliasing] of String = ( // texts used before version 1.647. the text "sine" should really be "sinc".
                                     'None','Low quality (image scaling using bilinear interpolation)','High quality (image scaling using a sine filter)');
  ApplicationHasBeenInstalledText= //'Sokoban YASC has been installed on the computer.';
                                   'Welcome to '+ApplicationName+' - Yet Another Sokoban Clone!'+NL+NL+
                                   'The setup has finished installing the application on your computer.';
  ApplicationIsAlreadyRunningText= 'This application is already running.'+NL+NL+
                                   'Do you want to open another instance?'+NL+NL+
                                   'Change'+NL+
                                   '''Settings | Control | Miscellaneous | Confirm multiple instances...'''+NL+
                                   'if you do not want to see this message again.';
  ApplicationIsBusyText          = 'The application it busy right now.'+NL+NL+
                                   'Please try again later.';
  ApplicationShutdownText        = 'This application is closing.';
  BackgroundColorText            = 'Background color';
  BackgroundText                 = 'Background';
  BestSolutionText__             = 'Best found solution: %d moves / %d pushes.';
  BestSolutionsText__            = 'Best found solutions: %d/%d and %d/%d.';
  BitmapImagesText               = 'Bitmap images';
  BoardText                      = 'Board';
  BoardDimensionsAsText          : array[TBoardDimensionsAsText] of String = (
                                     'Columns x Rows','Rows x Columns');
  BoardReconstructedFromMovesText= 'The board has been reconstructed from the moves and may differ from the original.';
  BooleanText                    : array[Boolean] of String = (TEXT_NO,TEXT_YES);
  BottomText                     = 'Bottom';
  BoxesText                      = 'Boxes';
  ButtonsText                    = 'Buttons';
  CalculatingImageText           = 'Calculating image. ';
  CalculatingPushesLowerBoundText
                                 = 'Calculating pushes lower bound';
  CalculationFailedBecauseOfNumericOverflowText
                                 = 'The calculation failed because of numeric overflow';
  CandidateText                  = 'Candidate';
  CandidateOrCandidatesSuffixText: array[Boolean] of String = ('candidate','candidates'); // 'False': singular; 'True': plural
  CandidatesCopiedToClipboardText__
                                 = '%d candidate(s) copied to the clipboard.';
  CandidatesCutToClipboardText__
                                 = '%d candidate(s) cut to the clipboard.';
  CandidatesImportedFromClipboardText__
                                 = '%d candidate(s) imported from the clipboard.'+NL+'Duplicates, if any, will be removed later';
  CandidateSetCaptionText        = 'Candidate Set';
  CandidateSetIsFullText         = 'The candidate set is full.'+NL+NL+'Please delete some of the existing candidates before you add new ones.';
  CannotSaveLevelToFileBecauseItContainsAnIdenticalLevelText__
                                 = 'The level cannot be saved to the file:'+NL+'"%s".'+NL+NL+'The file contains an identical level with the name:'+NL+'"%s".';
  CaptureChangedText             = 'The capture has been modified.';
  CaptureNewText                 = 'This is a new capture.';
  ChangedCandidateSetText        = 'The candidate set has been modified.';
  ChangedCandidateSetNewText     = 'This candidate set is new.';
  ChangedText                    = 'The level has been modified.'+NL+NL+DoYouWantToSaveItText;
  ChangedNewText                 = 'This level is new.'+NL+NL+DoYouWantToSaveItText;
  ChangeImageToBMPFormatText     = 'Do you want to change the image to a BMP image?';
  ChangingPluginText             = 'Changing plugin';
  CheckSkinTypeText              = 'Please check that the type of the skin matches the selected script.';
  ClearListQuestionText          = 'Do you want to clear the list?';
  ClickOpenText                  = 'Click "Open..." to select track';
  ClipboardItemText              = 'Clipboard Item';
  ClipboardNotASokobanGameText   = 'The clipboard does not contain valid Sokoban levels, or they do not conform to the format supported by this application.';
  ClipboardText                  = 'Clipboard';
  CollectionCopiedToClipboardText= 'The collection has been copied to the clipboard.';
  CollectionText                 = 'Collection';
  ColorPaletteFilesText          = 'Palette files';
  ColorThemeText                 : array[ TColorTheme ] of String =
                                 ( '', 'Blue', 'Green', 'Orange', 'Red' );
  ColumnsText                    = 'Columns';
  CombineWith_CTRL_ToSearchForUnsolvedLevelsText
                                 = '  (Combine with [Ctrl] to search for unsolved levels)';
  CombineWithOppositeDirectionSnapshotsToFormSolutionsText
                                 : array[Boolean] of String = ( // leading spaces must match the captions for panels (menu items) on 'MainForm.PanelMultiViewMenu'
                                   ' Combine with matching reverse mode snapshots to form solutions',
                                   ' Combine with matching forward snapshots to form solutions');
  CombineWith_SHIFT_ToAddAllSolutionsText
                                 = '  (Combine with [Ctrl] to add all solutions instead of just best solutions)';
  ContinuationMovesCopiedToClipboardText
                                 = 'The continuation moves have been copied to the clipboard.';
  CopiedBoardToClipboardText     = 'The board has been copied to the clipboard.';
  CopiedClipboardItemToClipboardText
                                 = 'The clipboard item has been copied to the clipboard.';
  CopiedSelectionToClipboardText = 'The selection has been copied to the clipboard';
  CopiedStatisticsToClipboardText= 'The statistics have been copied to the clipboard';
  CopiedToGeneratorText          = 'The board has been added to the level generator task queue. Duplicates, if any, will be removed later';
  CopyFileFailedText__           = 'Copy file failed:'+NL+'"%s".';
  CopyToClipboardText            = 'Copy to clipboard';
  CopyToClipboardFormatText      : array[Boolean] of String
                                 = ('Run-length encoded','Normal');
  CreateDirectoryFailedText__    = 'Create folder failed:'+NL+'"%s".';
  CurrentLevelCollectionText__   = 'Currently opened level collection: "%s"';
  CursorTypeText                 : array[TCursorType] of String =
                                   ('Arrow','Cross','Drag','Hand');
  DataTooLargeText__             = 'Data is too large:'+NL+'%s';
  DateText                       = 'Date';
  DeadlockDetectionCalculationInProgressTryAgainText
                                 = 'Deadlock detection calculation in progress. Please try again later';
  DeadlockDetectionMustBeEnabledText
                                 = 'Deadlock detection must be enabled, and the level size must not exceed the limits for deadlock detection calculations'; 
  DeadlockDetectionTypeText      : array[TLowMediumHigh] of String = (
  	                           'Low: Illegal squares + immovable boxes',
	                           'Medium: Low + deadlocks calculated at level load time',
	                           'High: Medium + searching each move for fenced-in areas'
                                   );
  DeadlockedPositionText         = 'This position is a deadlock.';
  DeadlockCalculationStatisticsText__
                                 = 'Deadlock sets: %d  Time: %d seconds';
  DeadlocksText                  = 'Deadlocks';
  DEFAULT_LEVEL_DIRECTORY        = 'Levels';
  DefaultSkinTitleText           = 'Games 4 Brains';
  DefaultText                    = 'Standard';
  DEFAULT_VALUE                  = LEFT_BRACKET+DefaultText+RIGHT_BRACKET;
  DeleteBestSolutionWarningText  = 'You are about to delete a best solution for this level.'+NL+NL+
                                   'Are you sure you want to delete this solution?'+NL+NL+
                                   'Please note that this warning will not appear again in this session.';
  DeleteCandidatesFromListText   = '&Delete candidates from list';
  DeleteFileQuestionText         = 'Do you want to delete the file';
  DeleteFilesQuestionText        = 'Do you want to delete the files';
  DeleteFilesText                = 'Delete Files';
  DeleteDirectoryFailedText__    = 'Delete folder failed:'+NL+'"%s".';
  DeleteSolutionText             = 'Delete solution';
  DirectionText                  : array[Boolean] of String = ('Forwards','Backwards');
  DirectoryExistsText__          = 'The folder'+NL+'"%s"'+NL+'already exists.';
  DirectoryMustBeEmptyBeforeDeletionText
                                 = 'Only empty and unprotected folders can be deleted.';
  DisabledEnabledText            : array[Boolean] of String =
                                     ('Disabled','Enabled');
  DiskFullText                   = 'Disk full.';
  DisplayDriverChangedActionText : array[TDisplayDriverChangedAction] of String =
                                     ('Shutdown','Continue on own risk');
  DummyLevelNotesLine1Text       = 'This is a dummy level.';
  DummyLevelNotesLine2Text       = 'Delete it after adding proper levels to the collection.';
  DuplicateFinderDidNotScanAnyFoldersText // must match menu item texts in the "Open" window
                                 = 'The duplicate finder did not scan any folders.'+NL+NL+
                                   'You can add/delete and enable/disable folders from the "Open Level" window, using the menu item group:'+NL+NL+
                                   '"Menu | Find duplicate levels... | Scanned folders...".'+NL+NL+
                                   'Selected folders have a check mark next to them.';
  DuplicateLevelsColumnHeadersText
                                 : array[TDuplicateLevelsStringGridColumn] of String
                                 = ('','Level','File','Folder');
  DuplicateLevelsStatusInfoText  : array[TDuplicateLevelsStatusInfoItem] of String
                                 = ('Scanning','      Folder','      File','Scanned','Levels','Duplicates','Files','Folders','Solutions');
  DuplicateLevelsStatusInfoFoldersText
                                 = '      Folders'; // the number of spaces must match the corresponding 'Folder' text in 'DuplicateLevelsStatusInfoText'
  DuplicateLevelsCaptionText     = 'Duplicate Levels';
  DuplicateLevelsReportFooterText= 'The numbers enclosed in parentheses are level numbers in the file';
  DuplicateLevelsText            = 'Duplicate levels';
  DuplicatesListCopiedToClipboardText
                                 = 'The current level list has been copied to the clipboard';
  EditorColorText                : array[TSquareColor] of String =
                                     ('Wall','Floor','Player','Box','Goal','BoxOnGoal');
  EditorHistoryFullText          = 'The editor history is full.'+NL+NL+
                                   'Please save your work and start working on another '+
                                   'level in order to empty the history.';
  EditorSkinsText                = 'Editor Skins';
  EitherNoSolutionOrNumericOverflowText
                                 = 'Either there is no solution, or the level is too large for the calculation to succeed.';
  EmptyFileNameText__            = 'The file name proper is empty:' + NL + NL +
                                   '"%s".' + NL + NL +
                                   'A full file name consists of a path, a file name proper, and an optional extension.';
  EnterButtonText                = '[Enter]';
  FileAssociationProgramNotFoundText__
                                 = 'Please note that file associations for ".sok" files and ".xsb" files were not updated.'+NL+NL+
                                   'The update requires an auxiliary service application that was not found on the computer:'+NL+
                                   '"%s".';
  FileConversionText             = 'File conversion';
  FileCopyFailedText__           = 'The file'+NL+'"%s"'+NL+NL+'could not be copied to'+NL+'"%s".';
  FileExistsText__               = 'The file'+NL+'"%s"'+NL+'already exists.';
  FileIOErrorText                = 'File I/O error';
  FileNameText                   = 'Filename';
  FileNameMissingText            = 'Filename is missing.';
  FileNotASokobanGameText__      = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain Sokoban levels, or they do not conform to the format supported by this application.';
  FileNotAGeneratorCandidateText__
                                 = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain a level generator candidate set, or it does not conform to the required syntax.';
  FileNotALegalCaptureToolEditorSkinText__
                                 = 'The image'+NL+'"%s"'+NL+
                                   'does not have the proper format for a capture tool skin. Please consult the default skin for proper format.';
  FileNotALegalImageFileText__   = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain an image of a type '+
                                   'supported by this application.';
  FileNotALegalImageListText__   = 'The image'+NL+'"%s"'+NL+
                                   'does not have the correct format for button images.'+NL+NL+
                                   'Each button image must be %d x %d pixels.';
  FileNotALegalOptimizerFileText__
                                 = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain an optimizer '+
                                   'supported by this application.';
  FileNotALegalPaletteFileText__ = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain a color palette of a type '+
                                   'supported by this application.';
  FileNotALegalSkinScriptText__  = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain a valid skin-type description.';
  FileNotALegalSolverFileText__  = 'The file'+NL+NL+'"%s"'+NL+NL+'does not contain a solver '+
                                   'supported by this application.';
  FileNotALegalToolsSkin1Text__  = 'Please note that the "Tools window" does not have the same flexible support for skins as the main window.'+NL+NL+
                                   'Skins for the "Tools window" must have a fixed format with 7 (square) tiles horizontally arranged:'+NL;
  FileNotALegalToolsSkin2Text__  = 'Player, player-on-goal, box, box-on-goal, goal, wall, and floor.'+NL+NL+
                                   'Each tile must be at least %d x %d pixels and the floor should preferably be blank, otherwise there is a severe performance penalty.'+NL+NL+
                                   'The image'+NL+'"%s"'+NL+
                                   'does not have the correct format for a "Tools window" skin.';
  FileNotCompressedWithRARorZIPText__
                                 = 'File is not a ".zip" compressed file or a ".rar" compressed file: '+NL+'"%s".';
  FileNotFoundText__             = 'The file'+NL+'"%s"'+NL+'does not exist or could not be opened.';
  FileNotSavedText__             = 'The file'+NL+'"%s"'+NL+'has not been saved.';
  FileWasRenamedText__           = 'A write error occurred during the operation.'+NL+NL+
                                   'As a result, the file'+NL+
                                   '"%s"'+NL+
                                   'has now been renamed to'+NL+
                                   '"%s".';
  FileListBoxHintText            ='|Select file from current folder  (Right-click to show menu)';
  FileRenameFailedText__         = 'The file'+NL+'"%s"'+NL+'could not be renamed to'+NL+'"%s".';
  FileTaskItemTypeText           : array[TFileTaskItemType] of String =
                                     ('','Folder','File','Level');
  FileTaskText__                 : array[TFileTaskType] of String =
                                     ('Copy %s','Move %s','Delete %s','Rename %s',
                                      'New folder','New level collection','New playlist','New textfile','Save as...');
  FireworksMenuText              : array[0..1] of String = (
                                   'Fireworks','Settings');
  FloorFillCharacter             : array[0..1] of String // don't localize
                                 = ('"-"','"_"'); // all items must be a single character enclosed in double-quotes
  FloorHintText                  = 'Tip: Left-clicking an empty square shows the boxes that can go there';
  FoldersText                    = 'Folders';
  FontText                       = 'Font';
  FpsText                        =  ' fps'; // note: SPACE required
  FractalsHelp1Text              = 'Select area: Click and drag the mouse';
  FractalsHelp2Text              = 'Zoom in: Click on rectangle    (Move selection: Click on rectangle and drag the mouse)';
  FractalsHelp3Text              = 'Resize selection: Click and drag the mouse';
  FractalsHelp4Text              = '    (Previous window: Right-click or Mousewheel down)';
  FractalsMenuText               : array[0..6] of String = (
                                   'Fractals','<','Reset','>','Colors...','Save as...','Settings');
  GameFilesText                  = 'Sokoban files';
  GameMetricsText                : array[TGameMetrics] of String = (
                                   'Moves','Pushes','Box lines','Box changes','Pushing sessions','Player lines');
  GameModeHintText               : array[Boolean] of String = (
                                   '|Change to reverse mode, i.e., solve the level backwards  (Ctrl+[Shift])',
                                   '|Change to normal mode, i.e., solve the level forwards  (Ctrl+[Shift])');
  GameSolvedText                 = 'The level has been solved. '; // SPACE required
  GenerateCandidateText          = 'Generate candidate';
  GenerateLevelsColumnHeadersText
                                 : array[TGenerateLevelColumn] of String
                                 = ('','',' Candidate','Pushes','Fitness','Birth','Children','Time'); // leading SPACE characters are important
  GeneratorButtonText            : array[TPluginButtonState] of String
                                 = ('Stop','&Terminating...','&Generate');
  GeneratorCallBackInfoText      : array[TGeneratorCallBackInfoItem] of String
                                 = ('Status','Positions','Open list','Time');
  GeneratorIsCurrentlyActivePleaseStopItBeforeYouAddNewCandidatesText
                                 = 'The generator is currently active.'+NL+NL+'Please stop it before you add new candidates.';
  GeneratorLimitsText__          = 'Limits:'+NL+
                                   '  Board width: %d'+NL+
                                   '  Board height: %d'+NL+
                                   '  Number of boxes: %d'+NL+
                                   '  Number of pushes: %d';
  GeneratorStatusInfoText        : array[TGeneratorStatusInfoItem] of String
                                 = ('Best','Candidates','Inactivity','Time','Memory full');
  GeneratorStatusTailPositionsSearchText
                                 = 'Tail positions search';
  GeneratorText                  = 'Generator';
  HeapStatisticsText             = 'Heap Statistics';
  HeapStatisticsFormatText__     = 'Free small:'+TAB+TAB+' %8d'+NL+
                                   'Free big:'+TAB+TAB+TAB+' %8d'+NL+
                                   ''+TAB+TAB+TAB+'-------------'+NL+
                                   'Total free:'+TAB+TAB+' %8d'+NL+
                                   'Overhead:'+TAB+TAB+' %8d'+NL+
                                   'Total allocated:'+TAB+TAB+' %8d'+NL+
                                   ''+TAB+TAB+TAB+'-------------'+NL+
                                   'Total committed:'+TAB+TAB+' %8d'+NL+
                                   'Total uncommitted:'+TAB+' %8d'+NL+
                                   ''+TAB+TAB+TAB+'-------------'+NL+
                                   'Total address space:'+TAB+' %8d'+NL+NL+
                                   'Available virtual address space:'+TAB+ '%8d';
  HelpText                       = 'Help';
  HiglightedBoxesCanMoveText     = 'Highlighted boxes can move';
  HiglightedBoxesCanMoveToTheSelectedSquareText
                                 = 'Highlighted boxes can move to the selected square';
  HintAddLevelSolutionsToTaskQueueText__
                                 = '|Add any remaining best solutions from the currently opened collection  (maximum %d solutions on the list)';
  //HintAddLevelsToTaskQueueText__ = '|Add any remaining levels from the currently opened collection to the task queue  (maximum %d levels on the queue)';
  HintAddLevelsToTaskQueueText__ = '|Add any remaining levels (max. %d) from the opened collection to the list';
  HintBetterBuiltInSolutionAvailableText
                                 = '.  (A shorter built-in solution is available via ''Open...'')';
  HintBookmarksLeftClickText     = '|Left-click: Make bookmark  (Ctrl+[Enter])';
  HintBookmarksRightClickText    = '.    Right-click: Back to move %d  ([Enter]).';
  HintBrowseHistoryText          = 'History  -  Click to select position';
  HintBuiltInSolutionAvailableText
                                 = '.  (A built-in solution is available via ''Open...'')';
  HintChangeColumnWidthText      = 'Click and drag the mouse to change column width';
  HintChangePanelHeightText      = 'Click and drag the mouse to change panel heights';
  HintChangePanelWidthText       = 'Click and drag the mouse to change panel widths';
  HintClickAndDragMouseToMoveSnapshotViewText
                                 = '.  Click and drag the mouse to move the view';
  HintClickOrPressEnterToSelectOptimizationText
                                 = 'Click or press [Enter] to select optimization';
  HintClickToSelectLevelText     = 'Left-click to select level.  Right-click to show menu';
  HintClickToSelectOptimizationText
                                 = 'Click to select optimization';
  HintClickToSelectPositionText  = 'Click to select position';
  HintClickToSelectSolutionText  = 'Left-click to select solution.  Right-click to show menu';
  HintCloseAboutWindowWhenYouAreDoneSoSuspendedToolsCanResumeTheirWorkText
                                 = 'Please close the "About" window as soon as you are done, so suspended tools can resume their work';
  HintCloseSettingsWindowWhenYouAreDoneSoSuspendedToolsCanResumeTheirWorkText
                                 = 'Please close the "Settings" window as soon as you are done, so suspended tools can resume their work';
  HintCloseViewText              : array[Boolean] of String = (
                                   'Close view',
                                   'Left-click: Move snapshot to "Snapshots" window.  Right-click: Close view.  (Duplicates are silently dropped)');
  HintCopyBitMapToClipboardText  = 'Copy from the canvas to the clipboard  (Ctrl+C)';
  HintCopyPuzzleToClipboardText  = 'Copy the level to the clipboard  (Ctrl+C)';
  HintCopySelectedSolutionToClipboard
                                 = '|Copy selected solution to the clipboard';
  HintCopySkinToClipboardText    = 'Copy the skin to the clipboard  (Ctrl+C)';
  HintCombineWithOppositeDirectionSnapshotsToFormSolutionsText
                                 : array[Boolean] of String = ( // leading spaces must match the captions for panels (menu items) on 'MainForm.PanelMultiViewMenu'
                                   '|Combine the snapshot with matching reverse mode snapshots to form solutions',
                                   '|Combine the snapshot with matching forward snapshots to form solutions');

  HintCopySolutionToClipboard    = '|Copy solution to the clipboard';
  HintDeleteRectText             = 'Delete section: Right-click';
  HintDeleteSelectedCandidatesFromListText
                                 = 'Delete|Delete the selected candidates from the list';
  HintDragRectText               = 'Move section: Click and drag the mouse';
  HintDrawingToolEraseBoxText    = '|Left-click to delete the box at this position';
  HintDrawingToolEraseGoalText   = '|Left-click to delete the goal at this position';
  HintDrawingToolRightClickEraseText
                                 = '.  Right-click to erase items, if any';
  HintDrawingToolRightClickMenuText
                                 = '.  Right-click to show menu';
  HintDrawingToolText            : array[TDrawingTool] of String =
                                   ('',
                                    '|Left-click to put a wall at this position',
                                    '|Left-click to put a box at this position',
                                    '|Left-click to put a goal at this position',
                                    '|Left-click to put the player at this position',
                                    '|Left-click to put a floor at this position',
                                    '|Click to erase any items at this position',
                                    '|Left-click and drag the mouse to select area'
                                   );
  HintDrawingToolMoveSelectedAreaText
                                 = '|To move the selected area, left-click inside the area and drag the mouse';
  HintDuplicateFinderSearchPathText
                                 : array[0..2] of String
                                 = ('The search scans the current folder, including subfolders',
                                    'The search scans the current folder and the '+ApplicationName+' level folder, including subfolders',
                                    'The search scans the '+ApplicationName+' level folder, including subfolders'
                                   );
  HintDuplicateFinderToolsMenuItemNewCollectionSolvedLevelsText
                                 : array[Boolean] of String
                                 = ('|Make copy of the processed level, including solutions from duplicates. Note that similar levels are not necessarily duplicates',
                                    '|Create new collection with solved levels found in this scan, including solutions from duplicates, if any'
                                   );
  HintDuplicateFinderToolsMenuItemUpdateSolutionsText
                                 : array[Boolean] of String
                                 = ('|Update solutions for duplicates of the processed level. Note that similar levels are not necessarily duplicates',
                                    '|Update solutions for duplicate levels found in this scan, i.e., distribute solutions to all identical levels'
                                   );
  HintDuplicateLevelsSelectLevelToSeeAThumbnailView
                                 = '|Left-click to select level. Right-click to open menu';
  HintFocusTaskText              = '|Click to focus this task in the table';
  HintGeneratorButton            : array[TPluginButtonState] of String
                                 = ('|Stop the level generator','|Stop now instead of waiting for the level generator to terminate normally','|Start the level generator');

  HintGeneratorSettings1Text     = '|Edit/view level generator settings';
  HintGeneratorSettings2Text     = HintGeneratorSettings1Text+'  (Changes will not take effect until next time the generator is activated)';

  HintImportBuiltInSolutionText__= '|Import built-in solution %d/%d';
  HintImportBuiltInSolutionsText__
                                 = '|Import built-in solutions %d/%d and %d/%d';
  HintInternalClipboardLeftClickText
                                 = '|Left-click: Save item on the internal clipboard';
  HintInternalClipboardRightClickText
                                 = '.    Right-click: Insert selected item.';
  HintInternetLevelLookupNormalizedBoardText
                                 : array[Boolean] of String
                                 = ('F5: Lookup board state: Unnormalized board','F5: Lookup board state: Normalized board');

  HintLeftClickToOpenCandidateSetText
                                 = '|Left-click to open this candidate set';
  HintLeftClickToOpenLevelText   = '|Left-click to open this level';
  HintMaximizeViewText           = 'Try to maximize view (Depending on the other views, this may leave the size unchanged)';
  HintMergeFilesText             = 'Select 2 or more level files to combine';
  HintMinimizeViewText           = 'Try to minimize view (Depending on the other views, this may leave the size unchanged)';
  HintMouseWheelChangesCaptureStepText
                                 = '. (Mouse wheel up/down while keeping the middle mouse button pressed can also change the capture step)';
  HintMouseWheelChangesDrawingToolText
                                 = '. (Mouse wheel up/down can also change the drawing tool)';
  HintMoveRectText               = '|Move selected area';
  HintNegativeValueMeansCenteredWallCapText
                                 = '  (a negative value means midways between the cells)';
  HintNoBuiltinSolutionsAvailableText
                                 = 'No built-in solutions available';
  HintNormalizeBoardText         = '|Fill jagged wall edges, remove redundant walls and floors, and remove unused boxes and goals';
  HintNormalizeBoardMakeRectangularBoardText
                                 = '|Like "Normalize board" except the board is filled up with outer walls to form a rectangle';
  HintNormalizeBoardSelectFilesText
                                 = 'Select level files to be normalized';
  HintNoSolutionAvailableText    = 'No solution available';
  HintOpenAnotherLevelLongText   = 'Select "Open", "<", or ">" to find another level.';
  HintOpenAnotherLevelShortText  = 'Select "Open" to find another level.';
  HintOpenCandidateSetText       = '|Open the candidate set';
  HintOpenLevelText              = '|Open the level';
  HintOpenNextLevelText          = '|Open next level from folder';
  HintOpenPreviousLevelText      = '|Open previous level from folder';
  HintOpenSnapshotButtonText     :array[Boolean] of String
                                 = ('|Open selected snapshot','|Open selected solution, making it ready for replay from the beginning');
  HintOpenSnapshotAsNewViewButtonText
                                 :array[Boolean] of String
                                 = ('|Open selected snapshot as a new view','|Open selected solution as a new view, making it ready for replay from the beginning');
  HintOpenSnapshot1Text          :array[Boolean] of String
                                 = ('|Click to open the snapshot','|Click to open the solution, making it ready for replay from the beginning');
  HintOpenSolverText             = '  (Open solver: Click the middle mouse button or press Ctrl+Q)';
  HintOptimizerButton            : array[TPluginButtonState] of String
                                 = ('|Terminate optimizer','|Stop now instead of waiting for the optimizer to terminate normally','|Try to optimize the selected solutions');
  HintPasteItemText              = 'Click to insert the item';
  HintPluginHasNoCancellationText= 'Please note that this plugin does not support cancellation.'+
                                   ' If it does not terminate by itself within a reasonable time'+
                                   ' and you terminate it manually, then this application closes'+
                                   ' in order to release any resources allocated by the plugin.';
  HintPluginReplayPauseText      : array[Boolean] of String
                                 = ('Replay|Replay game  (Ctrl+P)',
                                    'Pause|Pause  (Ctrl+P)');
  HintPluginSettings1Text        = '|Edit/view plugin settings';
  HintPluginSettings2Text        = HintPluginSettings1Text+'  (Changes may not take effect until next time the plugin is activated)';
  HintRemoveSelectedLevelsFromListText
                                 = 'Remove|Remove the selected levels from the list  (this does not delete the levels)';
  HintRemoveSelectedSolutionsFromListText
                                 = 'Remove|Remove the selected solutions from the list  (this does not delete the solutions)';
  HintReplayInToolsWindowText    = '|Left-click to replay game  (Ctrl+P)';
  HintReplaySolutionsForSolvedLevelsText
                                 = '  (Combine with [Shift+Ctrl] to replay solutions for solved levels)';
  HintResetInToolsWindowText     = '.    Right-click to restart game  ([Home])';
  HintResetRightClickText        = '.    Right-click:   ([Enter]).';
  HintResizeRectText             = 'Change section size: Click and drag the mouse';
  HintResizeRect2Text            = '|Resize selected area';
  HintResizeViewText             = 'Click and drag the mouse to resize view';
  HintResetOnHOMEandENTERtext    = 'Restart|Restart level  ([Home] or [Enter])';
  HintResetOnHOMEtext            = 'Restart|Restart level  ([Home])';
  HintRightClickIsAShortCutText  = '.  (Right-click is an easy shortcut.)';
  HintRightClickToReturnText     = '.  Right-click to cancel and return to the previous window';
  HintRightClickToShowMenuText   = 'Right-click to show menu';
  HintRightClickToSelectAreaText = '.    Right-click and drag the mouse to select/unselect area to be optimized';
  HintPlayCapturedLevelText      = 'Play the captured level (or an identical level) in the main window.';
  HintSaveAndPlayCapturedLevelText
                                 = 'Save the captured level and play it (or an identical level) in the main window.';
  HintFitnessText                = 'The fitness is an estimate of the complexity.  '; // trailing spaces important

  HintSelectOptimizationRangeText
                                 : array[ Boolean ] of String
                                 = ('Click and drag the mouse to select the start of the interval of pushes to be optimized (exclusive)',
                                    'Click and drag the mouse to select the end of the interval of pushes to be optimized (inclusive)');
  HintSelectRectText             = 'Select section: Mouse-click';
  HintSelectViewText             = 'Click the mouse to select the view';
  HintSelectZoomRectText         = 'Select section: Click and drag the mouse';
  HintSelectZoomRect2Text        = 'To resize the selected area, left-click and drag the mouse';
  HintShowMenuText               = 'Show menu';
  HintShowPluginInformationText  = '|Show information about this plugin';
  HintShowSolution1Text__        = '|Show solution %d/%d%s  (Ctrl+L)';
  HintShowSolutionMovesAndPushesText__
                                 = '|Left-click: Solution/moves %d/%d%s.   Right-click: Solution/pushes %d/%d%s.';
  HintShowSolutionPushesAndMovesText__
                                 = '|Left-click: Solution/pushes %d/%d%s.   Right-click: Solution/moves %d/%d%s.';
  HintSnapshotsText              = '|Open ''Snapshots window''  ([Shift])';
  HintSnapshotsLeftClickText     = '|Left-click: Save snapshot  (Ctrl+[Enter])';
  HintSnapshotsRightClickText    = '.    Right-click: Open selected snapshot ([Enter]).';
  HintSplitViewText              = 'Split view';
  HintSokobanForWindowsSelectFilesText
                                 = 'Select one or more "Sokoban for Windows" level files';
  HintSokofanSelectFilesText     = 'Select one or more "Sokofan" level files';
  HintSokoMindSelectFilesText    = 'Select one or more "SokoMind" level files';
  HintSolverButton               : array[TPluginButtonState] of String
                                 = ('|Terminate solver','|Stop now instead of waiting for the solver to terminate normally','|Try to solve level(s) using the selected solver');
  HintSortLevelsOnListText       = '|Sort levels on the list';
  HintSortSolutionsOnListText    = '|Sort solutions on the list';
  HintSortOnChronologicalOrderText
                                 = 'Left-click to sort on chronological order.  Right-click to show menu';
  HintSortOnColumnText           = 'Left-click to sort on column.  Right-click to show menu';
  HintStopDeadlockCalculationText= '. Click to stop the calculation  (Ctrl+I)';
  HintTaskQueueSelectItemText    = 'Click to toggle selection';
  HintToggleReplayDirectionText  = 'Toggle direction: Right-click';
  HintToolsWindowReplayText      = '  (Left-click to pause, right-click to toggle direction)';
  HintToolsWindowReplaySpeedText = '|Left-click and drag the mouse to change replay speed';
  HintUndoText                   = 'Undo|Undo last move  ([Delete] or [Backspace])';
  HintUnpackFileText             = 'Unpack file';
  HintUse_INSERT_And_DELETE_ToStepText
                                 = '.  (Use [Insert] and [Delete] to step through the game.)';
  HintUseMouseWheelToStepText    = '.  (Use mouse wheel up/down to step through the game.)';
  HuffmanBase64EncodedBoardCopiedToClipboardText
                                 = 'The Huffman-Base64 encoded board has been copied to the clipboard.';
  HuffmanBase64ImportFailedText
                                 = 'Importing a Huffman-Base64 encoded board from the clipboard failed.'+NL+NL+
                                   'Typically, the reason is that the clipboard does not contain a '+
                                   ' Huffman-Base64 encoded board.';
  HuffmanBase64ImportText        = 'Import Huffman-Base64 Encoded Board';
  HuffmanBase64EncodedBoardMessageText__
                                 = 'Huffman-Base64 encoded board:'+NL+NL+
                                   '%s'+NL+NL+
                                   'Board as uncompressed text: %d characters'+NL+NL+
                                   'Encoded size: %d characters = %d%%'+NL+NL+
                                   'Do you want to copy the encoded board to the clipboard?';
  HuffmanBase64EncodingText      = 'Huffman-Base64 Encoding';
  ImageText                      = 'Image';
  ImageViewText                  : array[TImageView] of String
                                 = ('None','Fill','Stretch', 'Center','Tile','Floor tile');
  ImageFilesText                 = 'Image files';
  ImageSectionText               = 'Section';
  ImageTextCaptionText           = 'Image Text';
  ImportFromClipboardText        = 'Import from clipboard';
  ImportTaskQueueText            = 'Import Task Queue';
  InBetweenFramesTypeText        : array[TInBetweenFramesType] of String
                                 = ('Duplicate','Interpolate');
  IncompleteListText             = '(Incomplete list)';
  InsertClipboardContents        = 'Insert clipboard contents';
  InternalClipboardText          = 'Internal Clipboard';
  InternalErrorText              = 'Internal error.';
  InternetLevelLookupText        = 'Internet Level Lookup';
  IntervalText                   = 'Interval';
  InvalidBoardInClipboardText__  = 'The clipboard does not seem to contain a legal board.'+NL+NL+
                                   'Clipboard contents:'+NL+NL+
                                   '"%s"'+NL+NL+
                                   'Information:'+NL+
                                   '%s';
  InvalidCompressedDataText      = 'The program could not decompress the data because the data seems to have a wrong format.';
  InvalidProperty1ValueText      = 'The property'+NL+'"%s"'+NL+'cannot be blank.'+NL+NL+TEXT_OLD_VALUE_IS_RESTORED;
  InvalidProperty2ValueText      = 'The property'+NL+'"%s"'+NL+
                                   'cannot have the value "%s".'+NL+NL+TEXT_OLD_VALUE_IS_RESTORED;
  IsReplayingText                : array[Boolean] of String
                                 = ('Replaying backwards...','Replaying forwards...');
  JumpText                       = 'Jump';
  LastSessionClosedUnexpectedlyText
                                 = 'The last session closed unexpectedly.';
  LastSessionClosedUnexpectedlyText2
                                 = 'Logged solutions from last session: %d.';
  LastSessionClosedUnexpectedlyText3
                                 = 'Logged solutions from last session that could not be assigned to levels: %d.'+NL+NL+
                                   'The reason is normally that levels have been moved, renamed, or changed after the solutions were logged.';
  LastSessionClosedUnexpectedlyText4
                                 = NL+NL+
                                   'Do you want to copy the orphaned solutions to the clipboard for later inspection?'+NL+NL+
                                   'Please note that this is a one-shot opportunity to get a copy of them, so it is highly recommended that you copy them to the clipboard.';
  LevelEditorText                = 'Level Editor';
  LevelExistsText__              = 'The level'+NL+'"%s"'+NL+'already exists.';
  LevelCollectionStatsText       = 'Level collection statistics: ';
  LevelCountText__               = '%d levels';
  LevelCollectionText            = 'Level collection';
  LevelCopiedToClipboardText     = 'The level has been copied to the clipboard.';
  LevelFolderText                = 'level folder';
  LocalDocumentsFolderText       = 'Local Documents';
  LogText                        = 'Log';
  LowerBoundText__               =  'Remaining pushes, lower bound: %d';
  MandalaMenuText                : array[0..4] of String = (
                                   'Mandala','Start','Save as...','Settings',StopText);
  MandalaStartStopHintText       : array[Boolean] of String =
                                   ('Start animation  ([Blank], [Enter], or mouse-click)',
                                    'Stop animation  ([Blank], [Enter], or mouse-click)');
  MandalaSubTitleText            = 'Images for contemplation...';
  MandelbrotText                 = 'Mandelbrot';
  MaxMovesExceededText           = 'The maximum number of moves cannot be exceeded.';
  MatchThresholdPercentText__    = 'Match threshold: %d%%';
  MediaDisappearedText__         = 'Drive %s: The media disappeared.'+NL+NL+
                                   'This window will not function properly '+
                                   'until you select a valid media.';
  MediaPlayerStateText           : array[TMPModes] of string
                                 = ('Not ready', 'Stopped', 'Playing...', 'Recording...', 'Seeking...', 'Pause', 'Open');
  MenuText                       = 'Menu';
  MetricsText                    = 'Metrics';
  MillionSuffixText              = ' million';       {must be lowercase}
  MinimumMovesText               = 'Minimum moves';  {don't translate to 'Optimal moves' ; even though they mean the same, they're used here simultaneously with different flavours}
  MinimumPushesText              = 'Minimum pushes'; {don't translate to 'Optimal pushes'; even though they mean the same, they're used here simultaneously with different flavours}
  Minimum2DSizeText__            = 'The size must be at least %dx%d';
  MoveLeadingToADeadlockPositionText
                                 = 'The move was blocked because it would lead to a deadlock position.';
  MPlayerImageViewerButtonHintText
                                 = 'Left-click: Full size.  Right-click: Select image';
  MoveOptimalSolutionText        = 'Move-optimal solution';
  MovePerSecondText              = 'move per second';
  MovesCopiedToClipboardText     = 'The moves have been copied to the clipboard.';
  MovesPerSecondText             = 'moves per second';
  MovePerSecondShortText         = ' move/second'; // leading space important
  MovesPerSecondShortText        = ' moves/second'; // leading space important
  MovesPushesText__              = '%d moves - %d pushes';
  MovesText                      = 'Moves';
  MusicFilesText                 = 'Music files';
  MusicPlayerColorText           : array[TRGBShift] of String = ('Blue','Green','Brown','Violet');
  MusicPlayerDirTaskText         : array[TMPlayerDirTask] of String = (
                                   'Select CD Audio drive',
                                   'Select folder',
                                   'Select playlist',
                                   'Create new playlist',
                                   'Save playlist',
                                   'Save playlist',
                                   'Select tracks to add to the playlist',
                                   'Select files to delete'
                                   );
  MusicPlayerTitleModeText       : array[TMPlayerTitleMode] of String = (
                                   'None',
                                   'Centered text - top','Centered text - bottom',
                                   'Scrolling text - top','Scrolling text - bottom');
  MusicPlayerText                : array[0..2*Succ((Ord(High(TMPlayerButtonType))-Ord(Low(TMPlayerButtonType))))-1] of String = (
                                   'Music Player','',
                                   'Display','',
                                   'Position','Select position', // button
                                   'Position','Select position', // bar
                                   '&Play', 'Start playing  (F6)',
                                   StopText, 'Stop playing  (Ctrl+F6)',
                                   'Previous track','Back to previous track  (F7)',
                                   'Next track', 'Forward to next track  (F8)',
                                   'Sound volume', 'Select sound volume', // button
                                   'Sound volume', 'Select sound volume', // bar
                                   'Open','Open track', // open
                                   'Close','Close this window, and return to Sokoban  ([Esc] or Alt+X)',
                                   'Help','Open help  (F1)',
                                   '&Pause', 'Pause  (F6)',
                                   'Menu','Show menu',
                                   'Previous activity','Select previous activity',
                                   'Next activity','Select next activity'
                                   );
  MusicPlayerTitleText           = 'Music Player';
  MusicSourceText                : array[TMusicSource] of String =
                                   ('CD Audio','Tracks from folder','Playlist');
  NameAlreadyExistsText__        = 'The name'+NL+'"%s"'+NL+
                                   'already exists.';
  NewBestSolutionsImportedText   = 'New best solution(s) imported.';
  NewFolderText                  = 'New folder';
  NewText                        = 'New';
  NoCandidatesFoundOnClipboardText
                                 = 'No candidates found on the clipboard.';
  NoLevelsFoundText              = 'No levels found.';
  NoMovesFoundText               = 'No moves found';
  NoOptimizationsFoundText       = 'No optimizations found'; // no trailing period because it's used in a status field where none of the other texts have one
  NoPackageLoadedText            = 'No Sokoban level collection is open.';
  NoSolutionsText                = 'No solutions';
  NotAnEditorHistoryFileText__   = 'Opening an editor logfile failed.'+NL+NL+
                                   'The file'+NL+'"%s"'+NL+
                                   'does not have the proper format for a '+
                                   'Sokoban editor logfile.';
  NotANumberText__               = '"%s" is not a number in the expected range.';
  NotAReverseModeSolutionBecauseThePlayerIsNotInTheRightZoneText
                                 = 'This is not a reverse mode solution because the player is not in the right zone';
  NotASettingsFileText__         = 'Loading settings from a file failed.'+NL+NL+
                                   'The file'+NL+'"%s"'+NL+
                                   'does not have the proper format for a '+
                                   'Sokoban settings file.';
  NoUndoText                     = 'Please note, there is no way back after this operation.';
  NumberExpectedText             = 'Number expected';
  NumberText                     = 'Number';
  NumberTooLargeText__           = 'Number too large: "%s"';
  OKChangedText                  : array[Boolean] of String
                                 = ('OK','Modified');
  OKText                         = 'OK';
  OnceRepeatText                 : array[Boolean] of String
                                 = ('Once','Repeat');
  OnlySavingImagesInBMPFormatIsSupportedText
                                 = 'This application can only save images in BMP format.';
  OnlyNonDefaultValuesText       = 'Only non-default values are listed.';
  OldWallTypeText                : array[TWallType] of String = ( // walltype names changed in version 1.309
                                   'Single wall','Seamless wall - no top','Seamless wall - with top');
  OnEscapeKeyActionText          : array[TOnEscapeKeyAction] of string
                                 = ('Exit application','Minimize window','Restart game (like [Home])','Do nothing');
  OnLeftClickEmptySquareActionText
                                 : array[TOnLeftClickEmptySquareAction] of String
                                 = ('Show boxes that can go to square','Move player to square, if possible');
  OnRightClickActionText         : array[TOnRightClickAction] of String
                                 = ('Load snapshot if any, else restart game','Restart game','Undo move');
  OnShiftKeyActionText           : array[TOnShiftKeyAction] of String
                                 = ('Open "Snapshots and Solutions" window','SHIFT->UP and UP->DOWN (ergonomically better for some keyboards with half-size UP and DOWN arrow keys)');
  OpenDialogFilterText           = 'Sokoban levels  (*'+SOKOBAN_FILE_NAME_EXT+';*'+XSB_FILE_NAME_EXT+')|*'+SOKOBAN_FILE_NAME_EXT+';*'+XSB_FILE_NAME_EXT+BAR+TextFilesFilterText+BAR+AllFilesFilterText;
  OpenFileFailedShortText__      = 'Open file failed:'+NL+
                                   '"%s".';
  OpenFileFailedLongText__       = 'Either %s does not contain any Sokoban levels, '+
                                   'or the application does not recognize the file format.'+NL+NL+
                                   'Please consult the built-in help for a description '+
                                   'of the Sokoban level syntax.';
  OpenFileText__                 = 'Opening "%s"...';
  OpenFormNotAvailableText       = 'The "Open" window is assigned to another task at the moment.'+NL+NL+
                                   'Please try again later, at a time when the "Settings" window has been opened directly from the main window.';
  OpenGameFileListBoxHintText    = '|Select a file with Sokoban levels  (Right-click to show menu)';
  OpenLevelOrGeneratorCandidateSetText
                                 = 'Open Level';
  OpenLevelsText                 = 'Open Levels';
  OpenPackFileMemberFailedText__ = 'Loading the level'+NL+'"%s"'+NL+
                                   'from the collection'+NL+'"%s"'+NL+'failed.';
  OpenPaletteText                = 'Open color palette';
  OpenPlayListText               = 'Open playlist';
  OpenSetCaptionText             = 'Open Set';
  OptimizerButtonText            : array[TPluginButtonState] of String
                                 = ('Stop','&Terminating...','Opti&mize');
  OptimizerText                  = 'Optimizer';
  OptimizeSolutionsColumnHeadersText
                                 : array[TOptimizeSolutionColumn] of String
                                 = ('','','Level','Solution','Metrics','Optimization');
  OptimizeSolutionText           = 'Optimize solution';
  OriginalFileText               = 'Original file';
  OverwriteItText                = 'Do you want to replace it?';
  OverwriteExistingItemText__    = '"%s"'+NL+'already exists.'+NL+NL+
                                   OverwriteItText;
  PackFileInvalidFormatText      = 'The file'+NL+'"%s"'+NL+'does not have the proper format '+
                                   'for a Sokoban level collection.';
  PackFileMemberNotFoundText__   = 'The level'+NL+'"%s"'+NL+'was not found in the collection'+NL+'"%s".';
  PackFileStringGridHintText     = '|Select level from the collection  (Right-click to show menu)';
  PaletteText                    = 'Palette';
  PassMofNText__                 = 'Pass %d of %d';
  PathFindingOptimizationTypeText: array[Boolean] of String = (
                                   'Minimize pushes','Minimize moves');
  PixelCodesNotInitializedText__ = '%s:'+NL+'Pixel codes must be initialized before use in'+NL+NL+'"%s".';
  PixelFormatErrorText           = 'This application handles images in 24-bit RGB format only.';
  PlayingFileText__              = 'Playing "%s"...';
  PlayListClearText              = 'Do you want to clear the playlist?';
  PlayListDeleteText             = 'Do you want to delete the playlist?';
  PlayListsText                  = 'Playlists';
  PlayListText                   = 'Playlist';
  PlayListFileNameText           = 'Playlist';
  PlayListStringGridHintText     = '|Select track from playlist';
  PleaseBacktrackText            = 'Please backtrack.';
  PleaseChangeItAndTryAgainText  = 'Please change it and try again.';
  PleaseCloseTheToolsWindowBeforeYouCloseTheApplicationText
                                 = 'Please close the "Tools" window before you close the application.';
  PleaseMakeTheNecessaryChangesText
                                 = 'Please make the necessary changes before the level is saved.';
  PleaseWaitText                 = 'Please wait...';
  PluginCallBackInfoText         : array[TPluginCallBackInfoItem] of String
                                 = ('Status','Moves','Pushes','Positions','Time');
  PluginIsBusyAndCannotChangeText__
                                 = 'The plugin "%s" is currently active.'+NL+NL+
                                   'To select another plugin, please terminate the current plugin and try again.';
  PluginLevelGroupBoxText        : array[Boolean] of String
                                 = ('Solve level','Solve levels');
  PluginLimitsText               = 'Limits';
  PluginResultText               : array[TPluginResult] of String
                                 = ('Solved',
                                    'Plugin limits preclude processing',
                                    'Invalid level',
                                    'Unsolvable',
                                    'Unsolved',
                                    'Game too long',
                                    'Invalid settings',
                                    'Failed',
                                    'Time limit exceeded',
                                    'Terminated by user');
  PluginsText                    = 'Plugins';
  PositionText                   = 'Position';
  PreparingListText              = 'Preparing list...';
  PullsText                      = 'Pulls';
  PushesText                     = 'Pushes';
  PushesOrMovesText              : array[Boolean] of String = (PushesText,MovesText);
  PushOptimalSolutionText        = 'Push-optimal solution';
  PushOrPushesSuffixText         : array[Boolean] of String = ('push','pushes'); // 'False': singular; 'True': plural
  PleaseReinstallText            = 'If this happens frequently, then please try to install the application again.';
  PressENTERText                 = 'Press [Enter]';
  RectangleIsOutOfBoundsText     = 'The rectangle is out of bounds.';
  RegistryKeyNotFoundText        = 'Key not found in the registry-database: '+NL;
  RemoveLevelsFromListText       = '&Remove levels from list';
  ReplayText                     = 'Replay';
  ReplaySpeedText                = 'Replay Speed';
  ReplaySpeed1Text               = 'Replay speed';
  ReservedNameText__             = '"%s" is a reserved name.';
  ReorganizingFileText           = 'Reorganizing file.';
  ResourceNotFoundText__         = 'The resource'+NL+'"%s", of type "%s",'+NL+'does not exist or could not be opened.';
  RightClickForMenuText          = '  (Right-click for menu)';
  RowsText                       = 'Rows';
  SaveFileFailedText__           = 'Saving the file failed:'+NL+
                                   '"%s".';
//SaveGameAvailableText          = 'Saved game: Replay moves using "Redo move" and "Redo all moves"';
  SaveCandidateSetCaptionText    = 'Save Candidate Set';
  SaveLevelCaptionText           = 'Save Level';
  SaveSettingsCaptionText        = 'Save Settings';
  SaveTaskQueueText              = 'Save Task Queue';
  ScanText                       = 'Scan';
  ScreenshotsText                = 'Screenshots';  
  ScriptTooDeeplyNestedText      = 'Script too deeply nested';
  SecondsFormatText__            = '%.2f seconds';
  SeeControlPathfindingSettingsText
                                 = 'See "Control | Pathfinding" settings';
  SelectMultipleFilesHint        = '|Select files - To select multiple files, keep [Ctrl] or [Shift] pressed while clicking the mouse';
  SelectMultipleTracksHint       = '|Select tracks - To select multiple tracks, keep [Ctrl] or [Shift] pressed while clicking the mouse';
  SelectSingleTrackHint          = '|Left-click to select track. Double-click or press [Enter] to play the selected track';
  SokobanFileNameStubText        = 'Soko';
  SolutionCopiedToClipboardText__= 'The solution %d/%d has been copied to the clipboard.';
  SolveLevelsColumnHeadersText   : array[TSolveLevelColumn] of String
                                 = ('','',' Level','Solution'); // SPACE before 'Level' is important
  SolverButtonText               : array[TPluginButtonState] of String
                                 = ('Stop','&Terminating...','Sol&ve');
  SettingsCopiedToClipboardText  = 'The settings have been copied to the clipboard';
  SettingsText                   = 'Settings';
  SettingsChangedText            = 'The settings have changed.'+NL+NL+'Do you want to apply the changes?';
  SkinExportFormatTypeText       : array[TSkinExportFormatType] of String = (
                                   '4 x 4','4 x 8'); // columns x rows
  SkinScriptExpectsAnImageText__ = 'The skin type'+NL+'"%s"'+NL+
                                   'expects an image as input, not a text-file.';
  SkinScriptExpectsAnINIFileText__
                                 = 'The skin type'+NL+'"%s"'+NL+
                                   'expects a text-file as input, not an image.';
  SkinsText                      = 'Skins';
  SkinScriptsText                = 'Skin scripts';
  SkinText                       = 'Skin';
  SkinTypesText                  = 'Skin types';
  SimilarLevelsText              = 'Similar levels';

//SolutionStepsText              = 'Solution - moves';
//SolutionPushesText             = 'Solution - pushes';
  SlideShowHintText              : array[Boolean] of String = (
                                   '|Start slide show','|Stop slide show');
  SnapshotCopiedToClipboardText  = 'Snapshot copied to clipboard';
  SnapshotInfoText__             : array[Boolean] of String
                                 = ('%d moves - %d pushes',
                                    '%d moves - %d pulls');
  SnapshotText                   = 'Snapshot';
  SokobanText                    = 'Sokoban'; // may be localized, in contrast to SokUtil_.TEXT_SOKOBAN
  SolutionsCopiedToClipboardText__
                                 = '%d solution(s) has been copied to the clipboard.';
  SolvedCountText__              = '%d solved';
  SnapshotOrSnapshotsSuffixText  : array[Boolean] of String = ('snapshot','snapshots'); // 'False': singular; 'True': plural
  SnapshotTypeAbbreviationText   : array[TSnapshotType] of Char
                                 = ('n','r','s',SPACE,'m','p','M','P','g','m');
  SolutionTypeText               : array[Boolean] of String = (
                                   'Solution - pushes', 'Solution - moves');
  SolveLevelText                 = 'Solve level';
  SolversText                    = 'Solvers';
  SolverText                     = 'Solver';
  SoundFilesText                 = 'Sound files';
  SoundText                      = 'Sound';
  SoundVolumeText                = 'Sound volume';
  StartingAnAlternateOptimizationFailedText
                                 = 'Launching an alternate optimization task failed. Please start the optimizer again.';  
  StatusBarHintText              : array[Boolean] of String
                                 = ('The numbers denote moves and pushes',
                                    'The numbers denote moves and pulls');
  StatusBarHintText__            = 'Box lines: %d  Box changes: %d  Pushing sessions: %d  Player lines: %d';
  TaskQueueText                  = 'Task Queue';
  TerminatePluginNowEvenThoughItMeansShuttingDownTheApplicationText
                                 = 'Stopping the plugin now, instead of waiting for it to terminate normally,'+
                                   ' will close this application in order to release any resources'+
                                   ' allocated by the plugin.'+NL+NL+
                                   'Do you want to terminate the plugin and close this application?';
  ThisMessageWillNotAppearAgainInThisSessionText
                                 = 'Please also note that this message will not appear again in this session.';
  SelectedSquaresText            = 'Selected squares';
  SyntaxErrorText__              = '%s:'+NL+'Unrecognized expression in'+NL+NL+'"%s".';
  TerminatedByUserText           = 'Terminated by user';
  TerminateTaskText              = 'The task has not finished yet. Are you sure you want to terminate it?';
  TextHasChangedText             = 'The text has changed.';
  TextOrNumberExpectedText       = 'Text or number expected';
  TextText                       = 'Text';
  TheFileText                    = 'the file';
  TheClipboardText               = 'the clipboard';
  TheCollectionText              = 'the level collection';
  ThisApplicationHandlesTheseImageFormatsOnlyText
                                 = 'This application handles images in BMP, JPG, JPEG, PNG (with limitations), WMF and EMF formats only.';
  ThreadPriorityText             : array[TThreadPriority] of String
                                 = ('Idle','Lowest','Lower','Normal','Higher','Highest','Time-critical');
  TimeStatisticsText             = 'Time Statistics';
  TimeText                       = 'Time';
  TimeOrTimesSuffixText          : array[Boolean] of String = ('time','times'); // 'False': singular; 'True': plural;
  TokenExpectedText__            = '"%s" expected in:'+NL+'"%s"';
  TooFewColorsText               = 'This application requires that the screen as a mimimum is set to 16-bit colors.'+NL+NL+
                                   'Please adjust the screen settings accordingly before using this application.';
  ToolsWindowSkinsText           = 'Tools Window Skins';
  ToolsText                      = 'Tools';
  TopText                        = 'Top';
  TrackText                      = 'Track';
  TransactionsText               = 'transactions';
  UnableToCalculateColumsAndRowsText
                                 = 'Calculating the number of columns and rows in the skin failed.';
  UndoRedoFailedText             = 'Undo/Redo failed.';
  UnknownCharText                = 'Unknown character in the level description: ';
  UnknownItemText__              = 'Unknown item in: "%s"';
  UnpackFileFailedText__         = 'Unpack file failed: '+NL+'"%s".';
  UnpackFileHintText__           = 'The file'+NL+NL+'"%s"'+NL+NL+'is a compressed file.'+NL+NL+
                                   'If you know it contains Sokoban level files you can click "Open" to let this application unpack it.';
  UnpackFileHint2Text            = NL+NL+
                                   'After unpacking, the following file conversions take place:'+NL+
                                   '* Levels in SokoMind format are converted to ".sok" format, '+
                                   'including solutions from all player accounts.'+NL+
                                   '* Levels in "Sokoban for Windows" format (".slc" files) are converted to ".sok" format, '+
                                   'including solutions.';
  UnpackFileHint3Text            = NL+
                                   '* Single level files with the file extension ".xsb" are merged to ".sok" collections.'+NL+
                                   '* Text files with levels and the file extension ".txt" are renamed to ".sok" files.'+NL+
                                   '* Empty folders, e.g., after the file conversions, are deleted.';
  UnpackedFilesText__            = '%d file(s) unpacked.';
  UnpackedFilesWithFailuresText__= '%d file(s) successfully unpacked, %d file(s) failed.'+NL+NL+'Failed on file:'+NL+'"%s".';
  UnpackFilesText                = 'Unpack Files';
  UnsolvableOrInvalidLevelAccordingToPluginText
                                 = ' Unsolvable or invalid level (according to plugin)'; // leading SPACE required
  UnsupportedImageFormatText     = 'Unsupported image type';
  UnsupportedImageFormatErrorText__
                                 = 'Opening the image'+NL+
                                   '"%s"'+NL+'failed.'+NL+NL+
                                   ThisApplicationHandlesTheseImageFormatsOnlyText+NL+NL+
                                   'Images in other formats must be converted to one of these formats before use.';
  UpdatedLevelsAndSolutionsText__= 'Updated levels: %d.'+NL+'Added solutions: %d.';
  UpdateItText                   = 'Do you want to update it?';
  UserDataMovedFromProgramFolderToUserFolderText__
                                 = 'Thank you for installing '+TEXT_APPLICATION_TITLE_LONG+PERIOD+NL+NL+
                                   'Please note that all levels, skins, and all other data files that used to be in the program folder:'+
                                   NL+'%s'+
                                   NL+'have moved to a new "Documents" subfolder for this application:'+
                                   NL+'%s.'+NL+NL+
                                   'You will not notice any difference when you use '+TEXT_APPLICATION_TITLE_LONG+PERIOD+
                                   ' It is only if you use other applications to access the files'+
                                   ' that you need to be aware of the transfer.';
  ViewDisappearedText            = 'Please note that 1 view has been moved to the "Snapshots" window because the application did not find room for it on the screen.';
  Viewer1ResizeToWindowWidthText : array[Boolean] of String = (
                                   'Height','Width');
  Viewer1ResizeToWindowWidthHintText
                                 : array[Boolean] of String = (
                                   '|Resize image to fit window width  (top and bottom clipped)',
                                   '|Resize image to fit window height  (complete image)');
  Viewer1MenuText                : array[0..7] of String = (
                                   'Images','Open...',' < ',' > ','Size:W/H','Settings','Slide show',StopText
                                   );
  ViewsDisappearedText__         = 'Please note that %d views have been moved to the "Snapshots" window because the application did not find room for them on the screen.';
  ViewText                       = 'View';
  WallHintText                   = 'Tip: Left-clicking a wall square highlights the movable boxes';
  WallTypeSeamlessWallsNoWallCapText
                                 = 'Seamless walls - no wall cap';
  WallTypeText                   : array[TWallType] of String = (
                                   'Tiled walls',WallTypeSeamlessWallsNoWallCapText,'Seamless walls - with wall cap');
  WindowsDisplayDriverChangedText= 'Windows'' display driver has changed.'+NL+'The program terminates now.'+NL+NL+
                                   'Unless the settings dictate otherwise, the current game will be saved, '+
                                   'and you can continue the game the next time the program is opened.'+NL+NL+
                                   'You can change this behavior in: '+NL+
                                   '''Settings | Control | Miscellaneous | On changing Windows'' display driver''.';
  WelcomeText                    = ' Welcome to '+ApplicationName+' - Yet Another Sokoban Clone!'; // leading space important, otherwise first character may not show up properly with small fonts; reason unknown
  VersionText                    = 'Version';
  TEXT_CONTINUE_TO_NEXT_STEP = 'Then continue to the next step.';
  TEXT_CAPTURE_APPLICATION_TITLE = 'Sokoban Level and Skin Capture';
  TEXT_CAPTURE_COPYRIGHT_HOLDER = 'Brian Damgard, Denmark';
  TEXT_CAPTURE_COPYRIGHT_YEAR = '2011-2012';
  TEXT_CAPTURE_EDITOR_SKIN_COPYRIGHT = 'Editor skin: Jumanji 3 copyright (c) by Gerry Wiseman';
  TEXT_DEFAULT_EDITOR_DEFAULT_SKIN_FILE_NAME = 'Sokoban YASC Capture Editor Skin';
  TEXT_CAPTURE_VERSION_NUMBER = '0.01';
  TEXT_CAPTURE_HISTORY_FILE_NAME = 'Sokoban';
  TEXT_ANALYSING_IMAGE_PLEASE_WAIT = 'Analysing image. Please wait...';
  TEXT_CAPTURE_HELP_1 =
    'This tool lets you capture Sokoban levels and skins from images. After loading the image, the capture is a three step process:'
    + NL + NL +
    '1. Specify where the level is in the image.'
    + NL + NL +
    '2. Specify the number of columns and rows in the level.'
    + NL + NL +
    '3. Specify the contents of each square in the level.';
  TEXT_CAPTURE_HELP_2 =
    NL + NL +
    'Now the level and the skin can be saved as files, or copied to the clipboard.';
  TEXT_CAPTURE_HELP_3 =
    SPACE +
    'The skin may require further processing, such as adding directional pusher images, or editing the wall cap if the level did not provide one. You can edit the skin in any image editor.';
  TEXT_COPYRIGHT = 'Copyright';
  TEXT_CREATE_SKIN = 'Create skin';
  TEXT_COLORS = 'colors';
  TEXT_EXTRACT_BOARD_USING_UNSCALED_SKIN_FAILED_HINT =
     'Please note that a common reason is the limitation, that as long as the board dimensions and the columns and rows have not been entered, the extraction only works when the skin has exactly the same scale as the level to be extracted from the image.';
  TEXT_FORMAT_BOXES_AND_GOALS_AND_PLAYERS = 'Boxes/goals/players: %d/%d/%d';
  TEXT_FORMAT_LOAD_EDITOR_SKIN_HINT =
    '|Load an image with editor skin and button images. Consult the default skin image for the proper format. You will find the default skin "[%s].bmp" in the default folder for this type of skins';
  TEXT_FORMAT_EXTRACT_BOARD_USING_SKIN_FAILED =
     'Using the skin' + NL + '"%s"' + NL + 'to extract a board from the image did not succeed.';
  TEXT_FORMAT_IMAGE_HAS_NO_SKIN_CAPTURE_INFORMATION =
    'The image' + NL + '"%s"' + NL + 'has no skin capture information.' + NL + NL +
    'Only previously captured skins can be used for extracting boards from images.';
  TEXT_FORMAT_IMAGE_IS_A_MATCHING_SKIN =
    'The file' + NL + '"%s"' + NL + 'contains a skin which matches the extracted level.' + NL + NL +
    'Do you want to update the existing skin?';
  TEXT_FORMAT_NEW_IMAGE_CONTAINS_SQUARE_TYPES_NOT_FOUND_IN_MATCHING_SKIN =
    'The image contains square types (e.g., "box-on-goal" and "player-on-floor") which were not available when the matching skin was extracted from another image.' + NL + NL +
    'Do you want to update the matching skin "%s" with the new square types?';
  TEXT_FORMAT_SELECT_BOARD_SIZE =
    'Move the mouse cursor to the %s of the board. Then click to select.' + NL +
    'In the selected rectangle, top and left lines are inclusive, whereas right and bottom lines are exclusive.';
  TEXT_HINT_CREATE_NEW_SKIN = 'Create a new skin instead of updating the current skin';
  TEXT_HINT_MATCHING_SKIN_EXTRACT_BOARD_FROM_IMAGE = '|Try to extract a board from the image by using a matching previously captured skin.';
  TEXT_HINT_MATCHING_SKIN_LOAD_SKIN = '|Load a matching previously captured skin.';
  TEXT_HINT_MATCHING_SKIN_LOAD_RECENTLY_USED_SKIN = '|Load one of the recently used captured skins.';
  TEXT_HINT_MATCHING_SKIN_MAY_BE_UPDATED = 'Note that the skin may be updated.';
  TEXT_HINT_MATCHING_SKIN_MAY_HAVE_DIFFERENT_DIMENSIONS  = 'The skin does not need to have the same scale as the level to be extracted from the image, because the board dimensions and the columns and rows have already been entered.';
  TEXT_HINT_MATCHING_SKIN_MUST_HAVE_IDENTICAL_DIMENSIONS = 'The skin must have exactly the same scale as the level to be extracted from the image, because the board dimensions and the columns and rows have not been entered yet.';
  TEXT_LOAD_IMAGE_CAPTION = 'Load Image';
  TEXT_LOAD_SKIN_CAPTION = 'Load Skin';
  TEXT_PUZZLE_EXAMPLE = 'Level example';
  TEXT_RECTANGLE_SIDE : array[ TDirection ] of String =
    ( 'top', 'left', 'bottom', 'right' );
  TEXT_SAVE_PUZZLE = 'Save level';
  TEXT_SAVE_PUZZLE_AS = 'Save the level under a new name';
  TEXT_SAVE_SKIN = 'Save skin';
  TEXT_SAVE_SKIN_AS = 'Save the skin under a new name';
  TEXT_SAVE_SKIN_CAPTION = 'Save Skin';
  TEXT_SCREEN_CAPTURE = 'Screen capture';
  TEXT_SCREEN_CAPTURE_FAILED = 'The screen capture failed. Instead, please take a screenshot manually and load it into the program by selecting "Paste" in the "Edit" menu.';
  TEXT_CAPTURE_STATE_HINT_BOARD_SIDE_1 =
    'Tip: You can use the keyboard arrow keys to nudge the lines into their proper positions. Later, you can use the spin edit controls.' + NL +
    'Tip: You can press Ctrl+A to select the entire image if the board fills the entire image.' + NL +
    'Tip: Try to mark the left and the top accurately, and overshoot the width and height a little if the board does not fill the entire image.';
  TEXT_CAPTURE_STATE_HINT_BOARD_SIDE_2 =
    'Tip: If you have a matching previously captured skin, you can try to use it for extracting the board from the image.';
  TEXT_CAPTURE_STATE_HINT_BOARD_SIDE_3 =
    '';
  TEXT_CAPTURE_STATE_HINT_ARROW_KEYS =
    'Tip: If you drag the grid with the mouse, you can use the keyboard arrow keys to nudge the grid into place.';
  TEXT_CAPTURE_STATE_HINT_GRID_LINES : String =
    'Move and click the mouse to mark the column width and the row height. ' + NL +
    'In the grid, top and left lines are inclusive, whereas right and bottom lines are exclusive.' + NL +
    'Tip: You can use the keyboard arrow keys to nudge the lines into their proper positions.';
  TEXT_CAPTURE_STATE_HINTS : array [ TCaptureStep ] of String =
  ( '',
    'Load an image with a Sokoban level from a file, from the clipboard, or capture the contents of the screen.',

    'Click and drag the mouse to select the board area. ' +
    TEXT_CONTINUE_TO_NEXT_STEP + NL +
    'In the selected rectangle, top and left lines are inclusive, whereas right and bottom lines are exclusive.' + NL +
    'Tip: Mark the top-left position accurately, and overshoot the width and height a little if the board does not fill the entire image.',
    'If necessary, adjust the grid position and the number of columns and rows. ' +
    TEXT_CONTINUE_TO_NEXT_STEP + NL +
    'In the grid, top and left lines are inclusive, whereas right and bottom lines are exclusive.' + NL +
    TEXT_CAPTURE_STATE_HINT_ARROW_KEYS,
    'Identify all the elements on the board, e.g., the walls, the floors, and the boxes. ' +
    TEXT_CONTINUE_TO_NEXT_STEP + NL +
    'Tip: It may be necessary to disable automatic completion in "Settings" in order to differentiate between elements at goal squares and at non-goal floor squares.',

    'The level can now be saved as a file, or copied to the clipboard. ' +
    'The next step offers the possibility to save the skin.' + NL +
    'Tip: If you plan to extract levels from other images having the same skin, you should save the skin the first time, so it can be used for subsequent extractions. ' +
    'Each time the skin is used for extracting levels with the same scale, the application collects and updates the skin with information about all seen square types. ' +
    'For instance, a level has always either a "pusher-on-floor" or a "pusher-on-goal", but not both. The application synthesizes the missing graphics until they can be harvested from another captured level. ' +
    'Note that existing skins first are updated automatically when you reach the "Level" step, where the level is guaranteed to be at least syntactically correct, and not while you still edit the board in the preceding "Squares" step.',

    'The skin can now be saved as a file, or copied to the clipboard. ' +
    'The skin may require further processing, such as adding directional pusher images, or editing the wall cap.'+
    'You can edit the skin in any image editor.' + NL +
    'Tip: If there are noise lines around the floors or the walls in the level example, you probably need to go back and adjust the top-left board position.'
  );

implementation

end.


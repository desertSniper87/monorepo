unit Options_;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Grids, Menus, Spin, ExtDlgs,
  Misc_, IniFile_, Text_, GView_;

{///$DEFINE MUSIC_PLAYER}

type
  TSettings            = (
                         stControl,
                         stMoveAnimation,
                         stMoveAnimationEnabled,
                         stSmoothMoveAnimationEnabled,
                         stPlayerDirectionAnimationEnabled,
                         stMoveAnimationDoMove,
                         stMoveAnimationUndoMove,
                         stMoveAnimationReplayMoves,
                         stMoveAnimationEnabledOnMouseWheelUpDown,
                         stMoveAnimationForkliftDriving,
                         stMoveAnimationForkliftDrivingEnabled,
                         stMoveAnimationForkliftDrivingDriveInReverseSquares,
                         stPathFinding,
                         stPathFindingMaxTime,
                         stPathFindingPushesLowerBoundCalculationMaxTime,
                         stPathFindingOptimizeMovesOrPushes,
                         stPathFindingShowLegalMoves,
                         stPathFindingShowLegalMovesEnabled,
                         stPathFindingShowLegalMovesSize,
                         stPathFindingShowLegalMovesTransparentImage,
                         stPathFindingShowLegalMovesTransparency,
                         stPathFindingShowLegalMovesPlayerCursor,
                         stPathFindingShowLegalMovesPlayerCursorEnabled,
                         stPathFindingShowLegalMovesPlayerCursorColor,
                         stPathFindingShowLegalMovesPlayerCursorPenWidth,
                         stPathFindingShowLegalMovesPlayerCursorSize,
                         stPathFindingShowLegalMovesPlayerCursorShadow,
                         stPathFindingShowLegalMovesPlayerCursorShadowEnabled,
                         stPathFindingShowLegalMovesPlayerCursorShadowColor,
                         stPathFindingShowLegalMovesBoxCursor,
                         stPathFindingShowLegalMovesBoxCursorEnabled,
                         stPathFindingShowLegalMovesBoxCursorColor,
                         stPathFindingShowLegalMovesBoxCursorPenWidth,
                         stPathFindingShowLegalMovesBoxCursorSize,
                         stPathFindingShowLegalMovesBoxCursorShadow,
                         stPathFindingShowLegalMovesBoxCursorShadowEnabled,
                         stPathFindingShowLegalMovesBoxCursorShadowColor,
                         stPathFindingShowLegalMovesDeadlocks,
                         stPathFindingShowLegalMovesDeadlocksEnabled,
                         stPathFindingShowLegalMovesDeadlocksColor,
                         stPathFindingShowLegalMovesJumpMoves,
                         stPathFindingShowLegalMovesJumpMovesEnabled,
                         stPathFindingShowLegalMovesJumpMovesColors,
                         stPathFindingShowLegalMovesJumpMovesColorsBackground,
                         stPathFindingShowLegalMovesJumpMovesColorsText,
                         stPathFindingShowLegalMovesJumpMovesColorsTextShadow,
                         stPathFindingShowLegalMovesAnimation,
                         stPathFindingShowLegalMovesAnimationPlayerAnimationEnabled,
                         stPathFindingShowLegalMovesAnimationBoxAnimationEnabled,
                         stPathFindingShowBoardCoordinates,
                         stSolutions,
                         stSolutionsPreferredType,
                         stSolutionsSaveAutomatically,
                         stSolutionsSaveOldSolutionsAfterFindingBetterOnes,
                         stSolutionsSaveOldSolutionsAfterClipboardImport,
                         stSolutionsRequireAtLeastOnePush,
                         stSolutionsAnimation,
                         stSolutionsAnimationPlayerAnimationEnabled,
                         stSolutionsAnimationBoxAnimationEnabled,
                         //stSolutionsScoreMetricsWeights,
                         //stSolutionsScoreMetricsWeightsBoxLines,
                         //stSolutionsScoreMetricsWeightsBoxChanges,
                         //stSolutionsScoreMetricsWeightsPushingSessions,
                         stSolutionsSecondaryMetricsInSolutionButtonHint,
                         stSolutionsAllowDeletionOfBestSolutionsInSnapshotsWindow,
                         stSolutionsErrorRecovery,
                         stSolutionsErrorRecoveryEnabled,
                         stSnapshots,
                         stSnapshotsSaveAutomatically,
                         stSnapshotsSecondaryMetricsInTitles,
                         stReverseMode,
                         stReverseModeShowBoxStartPositionsAsGoals,
                         stReverseModeJumpsAllowedAfterFirstBoxMove,
                         stDoAndUndoMoves,
                         stUndoRedoCombinedMoves,
                         stOptimizeMovesBetweenPushes,
                         stOnLeftClickEmptySquareAction,
                         stOnRightClickAction,
                         stOnShiftKeyAction,
                         stDeadlockDetection,
                         stDeadlockDetectionEnabled,
                         stDeadlockDetectionType,
                         stDeadlockDetectionBlockMoves,
                         stDeadlockDetectionLogEnabled,
                         stDeadlockDetectionShowSimpleDeadSquares,
                         stDeadlockDetectionShowSimpleDeadSquaresEnabled,
                         stDeadlockDetectionShowSimpleDeadSquaresColor,
                         stDeadlockDetectionShowSimpleDeadSquaresTransparencyPct,
                         stTiming,
                         stTimingEnabled,
                         stTimingIdleTimeThreshold,
                         stTimingIdleTimeThresholdEnabled,
                         stTimingIdleTimeThresholdSeconds,
                         stWindows,
                         stWindowsMain,
                         stWindowsMainLeft,
                         stWindowsMainTop,
                         stWindowsMainWidth,
                         stWindowsMainHeight,
                         stWindowsMainBoardDimensionsInTitleLine,
                         stWindowsMainCollectionNameInTitleLine,
                         stWindowsOpen,
                         stWindowsOpenLeft,
                         stWindowsOpenTop,
                         stWindowsOpenWidth,
                         stWindowsOpenHeight,
                         stWindowsOpenDiskGroupBox,
                         stWindowsOpenDiskGroupBoxWidth,
                         stWindowsOpenDiskGroupBoxHeight,
                         stWindowsOpenFolderListBox,
                         stWindowsOpenFolderListBoxWidth,
                         stWindowsOpenLevelPreview,
                         stWindowsOpenLevelPreviewHeight,
                         stWindowsOpenSolutionColumns,
                         stWindowsOpenIncludeBuiltinSolutions,
                         stWindowsOpenToolTips,
                         stWindowsOpenToolTipsEnabled,
                         stWindowsOpenToolTipsOffsetX,
                         stWindowsOpenToolTipsOffsetY,
                         stWindowsTools,
                         stWindowsToolsLeft,
                         stWindowsToolsTop,
                         stWindowsToolsWidth,
                         stWindowsToolsHeight,
                         stWindowsToolsCollectionNameInTitleLine,
                         stWindowsSnapshots,
                         stWindowsSnapshotsLeft,
                         stWindowsSnapshotsTop,
                         stWindowsSnapshotsWidth,
                         stWindowsSnapshotsHeight,
                         stWindowsInternalClipboard,
                         stWindowsInternalClipboardLeft,
                         stWindowsInternalClipboardTop,
                         stWindowsInternalClipboardWidth,
                         stWindowsInternalClipboardHeight,
                         stCursors,
                         stSelectionCursorType,
                         stDragCursorType,
                         stTrackingCursorType,
                         stTools,
                         stToolsSolver,
                         stToolsSolverFileName,
                         stToolsSolverPriority,
                         stToolsSolverAddPluginNameToSolutionTitles,
                         stToolsSolverAddDateToSolutionNotes,
                         stToolsSolverStatisticsEnabled,
                         stToolsSolverTimeLimit,
                         stToolsSolverTimeLimitEnabled,
                         stToolsSolverTimeLimitSeconds,
                         stToolsOptimizer,
                         stToolsOptimizerFileName,
                         stToolsOptimizerPriority,
                         stToolsOptimizerAddPluginNameToSolutionTitles,
                         stToolsOptimizerAddDateToSolutionNotes,
                         stToolsOptimizerStatisticsEnabled,
                         stToolsOptimizerDiscardDethronedSolutions,
                         stToolsOptimizerSeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly,
                         stToolsOptimizerTimeLimit,
                         stToolsOptimizerTimeLimitEnabled,
                         stToolsOptimizerTimeLimitSeconds,
                         stToolsOptimizerAlternatingOptimizations,
                         stToolsOptimizerAlternatingOptimizationsEnabled,
                         stToolsOptimizerAlternatingOptimizationsType,
                         stToolsOptimizerAlternatingOptimizationsTrigger,
                         stToolsOptimizerAlternatingOptimizationsRepeat,
                         stToolsOptimizerAlternatingOptimizationsRepeatEnabled,
                         stToolsOptimizerAlternatingOptimizationsRepeatType,
                         stToolsOptimizerIntervals,
                         stToolsOptimizerIntervalsWallifyBoxesOutsideRangeOfPushes,
                         stToolsOptimizerIntervalsSubIntervalOverlapPct,
                         stToolsGenerator,
                         stToolsGeneratorPriority,
                         stToolsCapture,
                         stToolsCaptureSnapToNearbyEdges,
                         stToolsCaptureGrid,
                         stToolsCaptureGridColor,
                         stToolsCaptureGridShadowColor,
                         stToolsCaptureTexts,
                         stToolsCaptureTextsDefaultSkinFileName,
                         stToolsCaptureAppendNewLevelsToCollections,
                         stToolsCaptureAppendNewLevelsToCollectionsCheckForDuplicates,
                         stToolsDuplicateFinder,
                         stToolsDuplicateFinderAdvancedIdentityCheck,
                         stToolsDuplicateFinderCurrentLevel,
                         stToolsDuplicateFinderCurrentLevelMatchThresholdPercent,
                         stToolsDuplicateFinderCurrentCollection,
                         stToolsDuplicateFinderCurrentCollectionNoOptions,
                         stToolsDuplicateFinderAllLevels,
                         stToolsDuplicateFinderAllLevelsNoOptions,
                         stToolsSkinExport,
                         stToolsSkinExportBaseFormatType,
                         stLevelFormat,
                         stAddFileFormatDescriptionToFiles,
                         stPrettyPrintGamesEnabled,
                         stLevelFormatCopyToClipboard,
                         stCopyLevelToClipboardFillFloors,
                         stCopyLevelToClipboardFloorFillCharacter,
                         stCopyLevelCollectionToClipboardIncludeLevelComments,
                         stCopyLevelToClipboardPreserveCombinedMoves,
                         stCopyLevelToClipboardBasedOnSolutionSliceIncludeTitles,
                         stRunLengthEncodingFloor,
                         stMiscellaneous,
//                       stAssociateFileTypes,
                         stRestoreSaveGameOnOpen,
                         stAutoAdvanceWhenSolved,
                         stMouseTrackingModeEnabled,
                         stBoardDimensionsAsText,
                         stBoardDimensionsWithFloorCount,
                         stOnEscapeKeyAction,
                         stDisplayDriverChangedAction,
                         stConfirmRunningMultipleInstances,
                         stScreenSaverEnabled,
                         stUseOnlyOneProcessor,
                         stSound,
                         stSoundEnabled,
                         stSoundVolume,
                         stSoundResetSoundEffectsOnLoadingASkin,
                         stSoundEffects,
                         stSoundMove, // caution: 'Move' and 'Push' must be the first two sound events
                         stSoundMoveEnabled,
                         stSoundMoveFileName,
                         stSoundPush,
                         stSoundPushEnabled,
                         stSoundPushFileName,
                         stSoundGoal,
                         stSoundGoalEnabled,
                         stSoundGoalFileName,
                         stSoundBlock,
                         stSoundBlockEnabled,
                         stSoundBlockFileName,
                         stSoundSolution,
                         stSoundSolutionEnabled,
                         stSoundSolutionFileName,
                         stSoundUndoMove,
                         stSoundUndoMoveEnabled,
                         stSoundUndoMoveFileName,
                         stSoundJump,
                         stSoundJumpEnabled,
                         stSoundJumpFileName,
                         stRestartGame,
                         stSoundRestartGameEnabled,
                         stSoundRestartGameFileName,
                         stSoundMenuButton,
                         stSoundMenuButtonEnabled,
                         stSoundMenuButtonFileName,
                         stSoundMenuSelection,
                         stSoundMenuSelectionEnabled,
                         stSoundMenuSelectionSoundFileName,
//                       stSoundFileTypeFilter,
{$IFDEF MUSIC_PLAYER}
                         stMusic,
                         stMusicEnabled,
                         stMusicSource,
                         stMusicCDDrive,
                         stMusicFilePath,
                         stMusicPlayListFileName,
                         stMusicFileTypeFilter,
                         stMusicPlayer,
                         stMusicPlayerTransparency,
                         stMusicPlayerTransparencyPct,
                         stMusicPlayerColor,
                         stMusicPlayerDisplay,
                         stMusicPlayerDisplayTransparencyPct,
                         stMusicPlayerDisplayFont,
                         stMusicPlayerDisplayFontName,
                         stMusicPlayerDisplayFontSize,
                         stMusicPlayerDisplayFontColor,
                         stMusicPlayerDisplayFontBold,
                         stMusicPlayerDisplayFontItalic,
                         stMusicPlayerDisplayFontUnderlined,
                         stMusicPlayerDisplayTitleMode,
                         stMusicPlayerDisplayShowFps,
                         stMusicPlayerDisplayActivities,
                         stMusicPlayerDisplayActivitiesActivity,
                         stMusicPlayerDisplayActivitiesActivities,
			 stMusicPlayerDisplayActivitiesActivitiesMandala,
			 stMusicPlayerDisplayActivitiesActivitiesMandalaEngine,
			 stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCycling,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMin,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMax,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingInterval,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingDeviation,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChange,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMin,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMax,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeInterval,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeDeviation,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffects,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsInterval,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsDeviation,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePattern,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternInterval,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternDeviation,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFade,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFadeSteps,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFadeStepsInterval,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternMorph,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternMorphStepsInterval,
			 stMusicPlayerDisplayActivitiesActivitiesMandalaEngineShowTitleOnStartup,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenu,
			 stMusicPlayerDisplayActivitiesActivitiesMandalaMenuTransparencyPct,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColors,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsBackground,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsFocusedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsGrayedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFont,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontName,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontSize,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontColor,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontBold,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontItalic,
                         stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontUnderlined,
			 stMusicPlayerDisplayActivitiesActivitiesImageViewer,
			 stMusicPlayerDisplayActivitiesActivitiesImageViewerFileName,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerResizeToWindowWidth,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShow,
			 stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowEnabled,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeDelayMS,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeFadeMS,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowRandomOrder,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenu,
			 stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuTransparencyPct,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColors,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsBackground,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsFocusedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsGrayedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFont,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontName,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontSize,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontColor,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontBold,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontItalic,
                         stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontUnderlined,
{
			 stMusicPlayerDisplayActivitiesActivitiesFireworks,
                         stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRockets,
                         stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsInterval,
                         stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsDeviation,
                         stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRockets,
                         stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsInterval,
                         stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsDeviation,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenu,
			 stMusicPlayerDisplayActivitiesActivitiesFireworksMenuTransparencyPct,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColors,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsBackground,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsFocusedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsGrayedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFont,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontName,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontSize,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontColor,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontBold,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontItalic,
                         stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontUnderlined,
}
			 stMusicPlayerDisplayActivitiesActivitiesFractals,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsPaletteFileName,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsAntiAliasing,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenu,
			 stMusicPlayerDisplayActivitiesActivitiesFractalsMenuTransparencyPct,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColors,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsBackground,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsFocusedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsGrayedButtonText,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsTextShadow,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFont,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontName,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontSize,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontColor,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontBold,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontItalic,
                         stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontUnderlined,
                         stMusicPlayerDisplayActivitiesMenu,
			 stMusicPlayerDisplayActivitiesMenuTransparencyPct,
                         stMusicPlayerDisplayActivitiesMenuTextShadow,
                         stMusicPlayerDisplayActivitiesMenuColors,
                         stMusicPlayerDisplayActivitiesMenuColorsBackground,
                         stMusicPlayerDisplayActivitiesMenuColorsButtonText,
                         stMusicPlayerDisplayActivitiesMenuColorsFocusedButtonText,
                         stMusicPlayerDisplayActivitiesMenuColorsGrayedButtonText,
                         stMusicPlayerDisplayActivitiesMenuColorsTextShadow,
                         stMusicPlayerDisplayActivitiesMenuFont,
                         stMusicPlayerDisplayActivitiesMenuFontName,
                         stMusicPlayerDisplayActivitiesMenuFontSize,
                         stMusicPlayerDisplayActivitiesMenuFontColor,
                         stMusicPlayerDisplayActivitiesMenuFontBold,
                         stMusicPlayerDisplayActivitiesMenuFontItalic,
                         stMusicPlayerDisplayActivitiesMenuFontUnderlined,
                         stMusicPlayerOpenWindow,
			 stMusicPlayerOpenWindowColors,
			 stMusicPlayerOpenWindowColorsBackground,
			 stMusicPlayerOpenWindowColorsBackgroundText,
                         stMusicPlayerOpenWindowColorsHighlightedText,
			 stMusicPlayerOpenWindowColorsButton,
                         stMusicPlayerOpenWindowColorsButtonText,
                         stMusicPlayerOpenWindowColorsFocusedButton,
                         stMusicPlayerOpenWindowColorsFocusedButtonText,
                         stMusicPlayerOpenWindowColorsGrayedButton,
                         stMusicPlayerOpenWindowColorsGrayedButtonText,
                         stMusicPlayerOpenWindowColorsWindow,
                         stMusicPlayerOpenWindowColorsWindowText,
                         stMusicPlayerOpenWindowColorsFocusedWindow,
                         stMusicPlayerOpenWindowColorsFocusedWindowText,
{$ENDIF}
                         stGraphics,
                         stGraphicsBackground,
                         stGraphicsBackgroundVisible,
                         stGraphicsBackgroundImage,
                         stGraphicsBackgroundImageFileName,
                         stGraphicsBackgroundImageSection,
                         stGraphicsBackgroundImageSectionLeft,
                         stGraphicsBackgroundImageSectionTop,
                         stGraphicsBackgroundImageSectionWidth,
                         stGraphicsBackgroundImageSectionHeight,
                         stGraphicsBackgroundImageView,
                         stGraphicsBackgroundImageAntialiasing,
                         stGraphicsBackgroundColor,
                         stGraphicsBoard,
                         stGraphicsBoardBackground,
                         stGraphicsBoardBackgroundVisible,
                         stGraphicsBoardBackgroundImage,
                         stGraphicsBoardBackgroundImageFileName,
                         stGraphicsBoardBackgroundImageSection,
                         stGraphicsBoardBackgroundImageSectionLeft,
                         stGraphicsBoardBackgroundImageSectionTop,
                         stGraphicsBoardBackgroundImageSectionWidth,
                         stGraphicsBoardBackgroundImageSectionHeight,
                         stGraphicsBoardBackgroundImageView,
                         stGraphicsBoardBackgroundImageAntialiasing,
                         stGraphicsBoardBackgroundBackgroundColor,
                         stGraphicsBoardBackgroundBackgroundColorTolerancePct,
                         stGraphicsBoardBackgroundColorMasked,
                         stGraphicsBoardBackgroundTransparency,
                         stGraphicsBoardBackgroundTransparencyPct,
                         stGraphicsBoardBackgroundColor,
                         stGraphicsBoardReverseModeBackground,
                         stGraphicsBoardReverseModeBackgroundVisible,
                         stGraphicsBoardReverseModeBackgroundImage,
                         stGraphicsBoardReverseModeBackgroundImageFileName,
                         stGraphicsBoardReverseModeBackgroundImageSection,
                         stGraphicsBoardReverseModeBackgroundImageSectionLeft,
                         stGraphicsBoardReverseModeBackgroundImageSectionTop,
                         stGraphicsBoardReverseModeBackgroundImageSectionWidth,
                         stGraphicsBoardReverseModeBackgroundImageSectionHeight,
                         stGraphicsBoardReverseModeBackgroundImageView,
                         stGraphicsBoardReverseModeBackgroundImageAntialiasing,
                         stGraphicsBoardReverseModeBackgroundBackgroundColor,
                         stGraphicsBoardReverseModeBackgroundBackgroundColorTolerancePct,
                         stGraphicsBoardReverseModeBackgroundColorMasked,
                         stGraphicsBoardReverseModeBackgroundTransparency,
                         stGraphicsBoardReverseModeBackgroundTransparencyPct,
                         stGraphicsBoardReverseModeBackgroundColor,
                         stGraphicsBoardReverseModeBackgroundPlayerStartPosition,
                         stGraphicsBoardReverseModeBackgroundPlayerStartPositionVisible,
                         stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridSize,
                         stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridColor,
                         stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridShadowColor,
                         stGraphicsBoardFigures,
                         stGraphicsBoardFiguresPlayer,
                         stGraphicsBoardFiguresPlayerImage,
                         stGraphicsBoardFiguresPlayerImageFileName,
                         stGraphicsBoardFiguresPlayerImageSection,
                         stGraphicsBoardFiguresPlayerImageSectionLeft,
                         stGraphicsBoardFiguresPlayerImageSectionTop,
                         stGraphicsBoardFiguresPlayerImageSectionWidth,
                         stGraphicsBoardFiguresPlayerImageSectionHeight,
                         stGraphicsBoardFiguresPlayerImageMasked,
                         stGraphicsBoardFiguresPlayerImageBackgroundColor,
                         stGraphicsBoardFiguresPlayerImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresPlayerImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresPlayerImageAntiAliasing,
                         stGraphicsBoardFiguresPlayerColor,
                         stGraphicsBoardFiguresPlayerAnimation,
                         stGraphicsBoardFiguresPlayerAnimationImage,
                         stGraphicsBoardFiguresPlayerAnimationImageFileName,
                         stGraphicsBoardFiguresPlayerAnimationImageSection,
                         stGraphicsBoardFiguresPlayerAnimationImageSectionLeft,
                         stGraphicsBoardFiguresPlayerAnimationImageSectionTop,
                         stGraphicsBoardFiguresPlayerAnimationImageSectionWidth,
                         stGraphicsBoardFiguresPlayerAnimationImageSectionHeight,
                         stGraphicsBoardFiguresPlayerAnimationImageMasked,
                         stGraphicsBoardFiguresPlayerAnimationImageBackgroundColor,
                         stGraphicsBoardFiguresPlayerAnimationImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresPlayerAnimationImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresPlayerAnimationImageAntiAliasing,
                         stGraphicsBoardFiguresPlayerAnimationInBetweenFrames,
                         stGraphicsBoardFiguresPlayerOnGoal,
                         stGraphicsBoardFiguresPlayerOnGoalImage,
                         stGraphicsBoardFiguresPlayerOnGoalImageFileName,
                         stGraphicsBoardFiguresPlayerOnGoalImageSection,
                         stGraphicsBoardFiguresPlayerOnGoalImageSectionLeft,
                         stGraphicsBoardFiguresPlayerOnGoalImageSectionTop,
                         stGraphicsBoardFiguresPlayerOnGoalImageSectionWidth,
                         stGraphicsBoardFiguresPlayerOnGoalImageSectionHeight,
                         stGraphicsBoardFiguresPlayerOnGoalImageMasked,
                         stGraphicsBoardFiguresPlayerOnGoalImageBackgroundColor,
                         stGraphicsBoardFiguresPlayerOnGoalImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresPlayerOnGoalImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresPlayerOnGoalImageAntiAliasing,
                         stGraphicsBoardFiguresPlayerOnGoalColor,
                         stGraphicsBoardFiguresPlayerOnGoalUsePlayerImageForMoveAnimation,
                         stGraphicsBoardFiguresPlayerOnGoalUsePlayerImageForHighlightingReachableSquares,
                         stGraphicsBoardFiguresPlayerOnGoalAnimation,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImage,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageFileName,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageSection,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageSectionLeft,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageSectionTop,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageSectionWidth,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageSectionHeight,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageMasked,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageBackgroundColor,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationImageAntiAliasing,
                         stGraphicsBoardFiguresPlayerOnGoalAnimationInBetweenFrames,
                         stGraphicsBoardFiguresBox,
                         stGraphicsBoardFiguresBoxImage,
                         stGraphicsBoardFiguresBoxImageFileName,
                         stGraphicsBoardFiguresBoxImageSection,
                         stGraphicsBoardFiguresBoxImageSectionLeft,
                         stGraphicsBoardFiguresBoxImageSectionTop,
                         stGraphicsBoardFiguresBoxImageSectionWidth,
                         stGraphicsBoardFiguresBoxImageSectionHeight,
                         stGraphicsBoardFiguresBoxImageMasked,
                         stGraphicsBoardFiguresBoxImageBackgroundColor,
                         stGraphicsBoardFiguresBoxImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresBoxImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresBoxImageAntiAliasing,
                         stGraphicsBoardFiguresBoxColor,
                         stGraphicsBoardFiguresBoxAnimation,
                         stGraphicsBoardFiguresBoxAnimationImage,
                         stGraphicsBoardFiguresBoxAnimationImageFileName,
                         stGraphicsBoardFiguresBoxAnimationImageSection,
                         stGraphicsBoardFiguresBoxAnimationImageSectionLeft,
                         stGraphicsBoardFiguresBoxAnimationImageSectionTop,
                         stGraphicsBoardFiguresBoxAnimationImageSectionWidth,
                         stGraphicsBoardFiguresBoxAnimationImageSectionHeight,
                         stGraphicsBoardFiguresBoxAnimationImageMasked,
                         stGraphicsBoardFiguresBoxAnimationImageBackgroundColor,
                         stGraphicsBoardFiguresBoxAnimationImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresBoxAnimationImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresBoxAnimationImageAntiAliasing,
                         stGraphicsBoardFiguresBoxAnimationInBetweenFrames,
                         stGraphicsBoardFiguresBoxOnGoal,
                         stGraphicsBoardFiguresBoxOnGoalImage,
                         stGraphicsBoardFiguresBoxOnGoalImageFileName,
                         stGraphicsBoardFiguresBoxOnGoalImageSection,
                         stGraphicsBoardFiguresBoxOnGoalImageSectionLeft,
                         stGraphicsBoardFiguresBoxOnGoalImageSectionTop,
                         stGraphicsBoardFiguresBoxOnGoalImageSectionWidth,
                         stGraphicsBoardFiguresBoxOnGoalImageSectionHeight,
                         stGraphicsBoardFiguresBoxOnGoalImageMasked,
                         stGraphicsBoardFiguresBoxOnGoalImageBackgroundColor,
                         stGraphicsBoardFiguresBoxOnGoalImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresBoxOnGoalImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresBoxOnGoalImageAntiAliasing,
                         stGraphicsBoardFiguresBoxOnGoalColor,
                         stGraphicsBoardFiguresBoxOnGoalUseBoxImageForMoveAnimation,
                         stGraphicsBoardFiguresBoxOnGoalUseBoxImageForHighlightingReachableSquares,
                         stGraphicsBoardFiguresBoxOnGoalAnimation,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImage,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageFileName,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageSection,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionLeft,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionTop,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionWidth,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionHeight,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageMasked,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageBackgroundColor,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresBoxOnGoalAnimationImageAntiAliasing,
                         stGraphicsBoardFiguresBoxOnGoalAnimationInBetweenFrames,
                         stGraphicsBoardFiguresGoal,
                         stGraphicsBoardFiguresGoalImage,
                         stGraphicsBoardFiguresGoalImageFileName,
                         stGraphicsBoardFiguresGoalImageSection,
                         stGraphicsBoardFiguresGoalImageSectionLeft,
                         stGraphicsBoardFiguresGoalImageSectionTop,
                         stGraphicsBoardFiguresGoalImageSectionWidth,
                         stGraphicsBoardFiguresGoalImageSectionHeight,
                         stGraphicsBoardFiguresGoalImageMasked,
                         stGraphicsBoardFiguresGoalImageBackgroundColor,
                         stGraphicsBoardFiguresGoalImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresGoalImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresGoalImageAntiAliasing,
                         stGraphicsBoardFiguresGoalImageTransparency,
                         stGraphicsBoardFiguresGoalImageTransparencyPct,
                         stGraphicsBoardFiguresGoalColor,
                         stGraphicsBoardFiguresWall,
                         stGraphicsBoardFiguresWallImage,
                         stGraphicsBoardFiguresWallImageFileName,
                         stGraphicsBoardFiguresWallImageSection,
                         stGraphicsBoardFiguresWallImageSectionLeft,
                         stGraphicsBoardFiguresWallImageSectionTop,
                         stGraphicsBoardFiguresWallImageSectionWidth,
                         stGraphicsBoardFiguresWallImageSectionHeight,
                         stGraphicsBoardFiguresWallImageMasked,
                         stGraphicsBoardFiguresWallImageBackgroundColor,
                         stGraphicsBoardFiguresWallImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresWallImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresWallImageAntiAliasing,
                         stGraphicsBoardFiguresWallColor,
                         stGraphicsBoardFiguresWallType,
                         stGraphicsBoardFiguresWallCap,
                         stGraphicsBoardFiguresWallCapLeft,
                         stGraphicsBoardFiguresWallCapTop,
                         stGraphicsBoardFiguresWallOuterWallTrimming,
                         stGraphicsBoardFiguresWallOuterWallTrimmingLeft,
                         stGraphicsBoardFiguresWallOuterWallTrimmingTop,
                         stGraphicsBoardFiguresWallOuterWallTrimmingRight,
                         stGraphicsBoardFiguresWallOuterWallTrimmingBottom,
                         stGraphicsBoardFiguresFloor,
                         stGraphicsBoardFiguresFloorImage,
                         stGraphicsBoardFiguresFloorImageFileName,
                         stGraphicsBoardFiguresFloorImageSection,
                         stGraphicsBoardFiguresFloorImageSectionLeft,
                         stGraphicsBoardFiguresFloorImageSectionTop,
                         stGraphicsBoardFiguresFloorImageSectionWidth,
                         stGraphicsBoardFiguresFloorImageSectionHeight,
                         stGraphicsBoardFiguresFloorImageMasked,
                         stGraphicsBoardFiguresFloorImageBackgroundColor,
                         stGraphicsBoardFiguresFloorImageBackgroundColorTolerancePct,
                         stGraphicsBoardFiguresFloorImageBackgroundColorMasksExteriorOnly,
                         stGraphicsBoardFiguresFloorImageAntiAliasing,
                         stGraphicsBoardFiguresFloorImageTransparency,
                         stGraphicsBoardFiguresFloorImageTransparencyPct,
                         stGraphicsBoardFiguresFloorColor,
                         stGraphicsBoardFiguresFloorVisible,
                         stGraphicsBoardFiguresMaxZoomFactorPct,
                         stGraphicsBoardGrid,
                         stGraphicsBoardGridEnabled,
                         stGraphicsBoardGridColor1,
                         stGraphicsBoardGridColor2,
                         stGraphicsMenu,
                         stGraphicsMenuTransparentButtons,
                         stGraphicsMenuTextShadow,
                         stGraphicsMenuButtons,
                         stGraphicsMenuDisabledButton,
                         stGraphicsMenuDisabledButtonImage,
                         stGraphicsMenuDisabledButtonImageFileName,
                         stGraphicsMenuDisabledButtonImageSection,
                         stGraphicsMenuDisabledButtonImageSectionLeft,
                         stGraphicsMenuDisabledButtonImageSectionTop,
                         stGraphicsMenuDisabledButtonImageSectionWidth,
                         stGraphicsMenuDisabledButtonImageSectionHeight,
                         stGraphicsMenuDisabledButtonImageBackgroundColor,
                         stGraphicsMenuDisabledButtonImageBackgroundColorTolerancePct,
                         stGraphicsMenuDisabledButtonMasked,
                         stGraphicsMenuDisabledButtonTransparencyPct,
                         stGraphicsMenuDisabledButtonFontColor,
                         stGraphicsMenuDisabledButtonTextShadowColor,
                         stGraphicsMenuEnabledButton,
                         stGraphicsMenuEnabledButtonImage,
                         stGraphicsMenuEnabledButtonImageFileName,
                         stGraphicsMenuEnabledButtonImageSection,
                         stGraphicsMenuEnabledButtonImageSectionLeft,
                         stGraphicsMenuEnabledButtonImageSectionTop,
                         stGraphicsMenuEnabledButtonImageSectionWidth,
                         stGraphicsMenuEnabledButtonImageSectionHeight,
                         stGraphicsMenuEnabledButtonImageBackgroundColor,
                         stGraphicsMenuEnabledButtonImageBackgroundColorTolerancePct,
                         stGraphicsMenuEnabledButtonMasked,
                         stGraphicsMenuEnabledButtonTransparencyPct,
                         stGraphicsMenuEnabledButtonFontColor,
                         stGraphicsMenuEnabledButtonTextShadowColor,
                         stGraphicsMenuFocusedEnabledButton,
                         stGraphicsMenuFocusedEnabledButtonImage,
                         stGraphicsMenuFocusedEnabledButtonImageFileName,
                         stGraphicsMenuFocusedEnabledButtonImageSection,
                         stGraphicsMenuFocusedEnabledButtonImageSectionLeft,
                         stGraphicsMenuFocusedEnabledButtonImageSectionTop,
                         stGraphicsMenuFocusedEnabledButtonImageSectionWidth,
                         stGraphicsMenuFocusedEnabledButtonImageSectionHeight,
                         stGraphicsMenuFocusedEnabledButtonImageBackgroundColor,
                         stGraphicsMenuFocusedEnabledButtonImageBackgroundColorTolerancePct,
                         stGraphicsMenuFocusedEnabledButtonMasked,
                         stGraphicsMenuFocusedEnabledButtonTransparencyPct,
                         stGraphicsMenuFocusedEnabledButtonFontColor,
                         stGraphicsMenuFocusedEnabledButtonTextShadowColor,
                         stGraphicsMenuFocusedDisabledButton,
                         stGraphicsMenuFocusedDisabledButtonImage,
                         stGraphicsMenuFocusedDisabledButtonImageFileName,
                         stGraphicsMenuFocusedDisabledButtonImageSection,
                         stGraphicsMenuFocusedDisabledButtonImageSectionLeft,
                         stGraphicsMenuFocusedDisabledButtonImageSectionTop,
                         stGraphicsMenuFocusedDisabledButtonImageSectionWidth,
                         stGraphicsMenuFocusedDisabledButtonImageSectionHeight,
                         stGraphicsMenuFocusedDisabledButtonImageBackgroundColor,
                         stGraphicsMenuFocusedDisabledButtonImageBackgroundColorTolerancePct,
                         stGraphicsMenuFocusedDisabledButtonMasked,
                         stGraphicsMenuFocusedDisabledButtonTransparencyPct,
                         stGraphicsMenuFocusedDisabledButtonFontColor,
                         stGraphicsMenuFocusedDisabledButtonTextShadowColor,
                         stGraphicsMenuButtonsEdges,
                         stGraphicsMenuButtonsEdgesEdgeSmoothing,
                         stGraphicsMenuButtonsEdgesTransparencyPct,
                         stGraphicsMenuButtonsFont,
                         stGraphicsMenuButtonsFontName,
                         stGraphicsMenuButtonsFontSize,
                         stGraphicsMenuButtonsFontColor,
                         stGraphicsMenuButtonsFontBold,
                         stGraphicsMenuButtonsFontItalic,
                         stGraphicsMenuButtonsFontUnderlined,
                         stGraphicsMenuPopupMenu,
			 stGraphicsMenuPopupMenuColors,
			 stGraphicsMenuPopupMenuColorsBackground,
			 stGraphicsMenuPopupMenuColorsBackgroundText,
                         stGraphicsMenuPopupMenuColorsHighlightedText,
			 stGraphicsMenuPopupMenuColorsButton,
                         stGraphicsMenuPopupMenuColorsButtonText,
                         stGraphicsMenuPopupMenuColorsFocusedButton,
                         stGraphicsMenuPopupMenuColorsFocusedButtonText,
                         stGraphicsMenuPopupMenuColorsGrayedButton,
                         stGraphicsMenuPopupMenuColorsGrayedButtonText,
                         stGraphicsMenuPopupMenuColorsWindow,
                         stGraphicsMenuPopupMenuColorsWindowText,
                         stGraphicsMenuPopupMenuColorsFocusedWindow,
                         stGraphicsMenuPopupMenuColorsFocusedWindowText,
                         stGraphicsStatus,
                         stGraphicsStatusImage,
                         stGraphicsStatusImageFileName,
                         stGraphicsStatusImageSection,
                         stGraphicsStatusImageSectionLeft,
                         stGraphicsStatusImageSectionTop,
                         stGraphicsStatusImageSectionWidth,
                         stGraphicsStatusImageSectionHeight,
                         stGraphicsStatusImageSectionBackgroundColor,
                         stGraphicsStatusImageSectionBackgroundColorTolerancePct,
                         stGraphicsStatusMasked,
                         stGraphicsStatusTransparencyPct,
                         stGraphicsStatusEdges,
                         stGraphicsStatusEdgesEdgeSmoothing,
                         stGraphicsStatusEdgesTransparencyPct,
                         stGraphicsStatusImageSlices,
                         stGraphicsStatusImageSlicesLeft,
                         stGraphicsStatusImageSlicesBody,
                         stGraphicsStatusImageSlicesRight,
                         stGraphicsStatusButtons,
                         stGraphicsStatusButtonTransparencyPct,
                         stGraphicsStatusPanels,
                         stGraphicsStatusPanelsBounds,
                         stGraphicsStatusPanelsBoundsTop,
                         stGraphicsStatusPanelsBoundsBottom,
                         stGraphicsStatusPanelsColor,
                         stGraphicsStatusPanelsTransparencyPct,
                         stGraphicsStatusPanelsRoundingRectanglePixels,
                         stGraphicsStatusFont,
                         stGraphicsStatusFontName,
                         stGraphicsStatusFontSize,
                         stGraphicsStatusFontColor,
                         stGraphicsStatusFontBold,
                         stGraphicsStatusFontItalic,
                         stGraphicsStatusFontUnderlined,
                         stGraphicsStatusSubTexts,
                         stGraphicsStatusSubTextsAreTransparent,
                         stGraphicsStatusSubTextsFontColor,
                         stGraphicsStatusSubTextsFontSize,
                         stGraphicsStatusTextShadow,
                         stGraphicsStatusTextShadowColor,
                         stGraphicsMultipleViews,
                         stGraphicsMultipleViewsColors,
                         stGraphicsMultipleViewsColorsBackground,
                         stGraphicsMultipleViewsColorsFocusedBackground,
                         stGraphicsMultipleViewsColorsLine,
                         stGraphicsMultipleViewsColorsText,
                         stGraphicsMultipleViewsColorsFocusedText,
                         stGraphicsMultipleViewsColorsShadow,
                         stGraphicsMultipleViewsBackgroundTransparencyPct,
                         stGraphicsMultipleViewsFont,
                         stGraphicsMultipleViewsFontName,
                         stGraphicsMultipleViewsFontSize,
                         stGraphicsMultipleViewsFontColor,
                         stGraphicsMultipleViewsFontBold,
                         stGraphicsMultipleViewsFontItalic,
                         stGraphicsMultipleViewsFontUnderlined,
                         stGraphicsTools,
                         stGraphicsToolsWindowFont,
                         stGraphicsToolsWindowFontName,
                         stGraphicsToolsWindowFontSize,
                         stGraphicsToolsWindowFontColor,
                         stGraphicsToolsWindowFontBold,
                         stGraphicsToolsWindowFontItalic,
                         stGraphicsToolsWindowFontUnderlined,
                         stGraphicsToolsButtons,
                         stGraphicsToolsButtonsImage,
                         stGraphicsToolsButtonsImageFileName,
                         stGraphicsToolsBoard,
                         stGraphicsToolsBoardBackground,
                         stGraphicsToolsBoardBackgroundVisible,
                         stGraphicsToolsBoardBackgroundImage,
                         stGraphicsToolsBoardBackgroundImageFileName,
                         stGraphicsToolsBoardBackgroundImageView,
                         stGraphicsToolsBoardBackgroundImageAntialiasing,
                         stGraphicsToolsBoardBackgroundColor,
                         stGraphicsToolsBoardSkin,
                         stGraphicsToolsBoardSkinImage,
                         stGraphicsToolsBoardSkinImageFileName,
                         stGraphicsToolsBoardSkinImageMasked,
                         stGraphicsToolsBoardSkinImageBackgroundColor,
                         stGraphicsToolsBoardSkinImageBackgroundColorTolerancePct,
                         stGraphicsToolsBoardSkinImageAntiAliasing,
                         stGraphicsToolsBoardSquareSet,
                         stGraphicsToolsBoardSquareSetSelectedSquaresColor,
                         stGraphicsToolsBoardSquareSetNotSelectedSquaresColor,
                         stGraphicsToolsBoardSquareSetTransparencyPct,
                         stGraphicsToolsEditor,
                         stGraphicsToolsEditorCursors,
                         stGraphicsToolsEditorCursorsCellCursor,
                         stGraphicsToolsEditorCursorsCellCursorPenColor,
                         stGraphicsToolsEditorCursorsCellCursorPenWidth,
                         stGraphicsToolsEditorCursorsCellCursorShadowColor,
                         stGraphicsToolsEditorCursorsEraserCursor,
                         stGraphicsToolsEditorCursorsEraserCursorPenColor,
                         stGraphicsToolsEditorCursorsEraserCursorPenWidth,
                         stGraphicsToolsEditorCursorsEraserCursorShadowColor,
                         stGraphicsToolsEditorCursorsSelectionCursor,
                         stGraphicsToolsEditorCursorsSelectionCursorPenColor,
                         stGraphicsToolsEditorCursorsSelectionCursorPenWidth,
                         stGraphicsToolsEditorCursorsSelectionCursorShadowColor,
                         stGraphicsToolsEditorFrame,
                         stGraphicsToolsEditorFrameColor,
                         stGraphicsToolsEditorFrameShadowColor,
                         stGraphicsToolsEditorGrid,
                         stGraphicsToolsEditorGridEnabled,
                         stGraphicsToolsReplay,
                         stGraphicsToolsReplaySpeedTrackBarColors,
                         stGraphicsToolsReplaySpeedTrackBarColorsBackgroundColor,
                         stGraphicsToolsReplaySpeedTrackBarColorsSliderColor,
                         stGraphicsToolsReplaySpeedTrackBarColorsFontColor,
                         stGraphicsToolsReplaySpeedTrackBarColorsShadowColor,
                         stGraphicsToolsInternalClipboard,
                         stGraphicsToolsInternalClipboardWindowFont,
                         stGraphicsToolsInternalClipboardWindowFontName,
                         stGraphicsToolsInternalClipboardWindowFontSize,
                         stGraphicsToolsInternalClipboardWindowFontColor,
                         stGraphicsToolsInternalClipboardWindowFontBold,
                         stGraphicsToolsInternalClipboardWindowFontItalic,
                         stGraphicsToolsInternalClipboardWindowFontUnderlined,
			 stGraphicsToolsInternalClipboardColors,
			 stGraphicsToolsInternalClipboardColorsBackground,
			 stGraphicsToolsInternalClipboardColorsBackgroundText,
                         stGraphicsToolsInternalClipboardColorsHighlightedText,
			 stGraphicsToolsInternalClipboardColorsButton,
                         stGraphicsToolsInternalClipboardColorsButtonText,
                         stGraphicsToolsInternalClipboardColorsFocusedButton,
                         stGraphicsToolsInternalClipboardColorsFocusedButtonText,
                         stGraphicsToolsInternalClipboardColorsGrayedButton,
                         stGraphicsToolsInternalClipboardColorsGrayedButtonText,
                         stGraphicsToolsInternalClipboardColorsWindow,
                         stGraphicsToolsInternalClipboardColorsWindowText,
                         stGraphicsToolsInternalClipboardColorsFocusedWindow,
                         stGraphicsToolsInternalClipboardColorsFocusedWindowText,
                         stGraphicsToolsInternalClipboardColorsNotesWindow,
                         stGraphicsToolsInternalClipboardEditFont,
                         stGraphicsToolsInternalClipboardEditFontName,
                         stGraphicsToolsInternalClipboardEditFontSize,
                         stGraphicsToolsInternalClipboardEditFontColor,
                         stGraphicsToolsInternalClipboardEditFontBold,
                         stGraphicsToolsInternalClipboardEditFontItalic,
                         stGraphicsToolsInternalClipboardEditFontUnderlined,
                         stGraphicsToolsInternalClipboardTexts,
                         stGraphicsToolsInternalClipboardTextsClipboardItemName,
                         stGraphicsToolsDuplicateLevels,
                         stGraphicsToolsDuplicateLevelsColors,
                         stGraphicsToolsDuplicateLevelsColorsBackgroundColor1,
                         stGraphicsToolsDuplicateLevelsColorsTextColor1,
                         stGraphicsToolsDuplicateLevelsColorsBackgroundColor2,
                         stGraphicsToolsDuplicateLevelsColorsTextColor2,
                         stGraphicsToolsDuplicateLevelsColorsHighlightBackgroundColor,
                         stGraphicsToolsDuplicateLevelsColorsHighlightTextColor,
                         stGraphicsSnapshots,
                         stGraphicsSnapshotsWindowFont,
                         stGraphicsSnapshotsWindowFontName,
                         stGraphicsSnapshotsWindowFontSize,
                         stGraphicsSnapshotsWindowFontColor,
                         stGraphicsSnapshotsWindowFontBold,
                         stGraphicsSnapshotsWindowFontItalic,
                         stGraphicsSnapshotsWindowFontUnderlined,
			 stGraphicsSnapshotsColors,
			 stGraphicsSnapshotsColorsBackground,
			 stGraphicsSnapshotsColorsBackgroundText,
                         stGraphicsSnapshotsColorsHighlightedText,
			 stGraphicsSnapshotsColorsButton,
                         stGraphicsSnapshotsColorsButtonText,
                         stGraphicsSnapshotsColorsFocusedButton,
                         stGraphicsSnapshotsColorsFocusedButtonText,
                         stGraphicsSnapshotsColorsGrayedButton,
                         stGraphicsSnapshotsColorsGrayedButtonText,
                         stGraphicsSnapshotsColorsWindow,
                         stGraphicsSnapshotsColorsWindowText,
                         stGraphicsSnapshotsColorsFocusedWindow,
                         stGraphicsSnapshotsColorsFocusedWindowText,
                         stGraphicsSnapshotsColorsNotesWindow,
                         stGraphicsSnapshotsEditFont,
                         stGraphicsSnapshotsEditFontName,
                         stGraphicsSnapshotsEditFontSize,
                         stGraphicsSnapshotsEditFontColor,
                         stGraphicsSnapshotsEditFontBold,
                         stGraphicsSnapshotsEditFontItalic,
                         stGraphicsSnapshotsEditFontUnderlined,
                         stGraphicsSnapshotsTexts,
                         stGraphicsSnapshotsTextsSolution,
                         stGraphicsSnapshotsTextsBestSolution,
                         stGraphicsSnapshotsTextsBestSolutionMoves,
                         stGraphicsSnapshotsTextsBestSolutionPushes,
                         stGraphicsSnapshotsTextsNormalModeSnapshotName,
                         stGraphicsSnapshotsTextsReverseModeSnapshotName,
                         stGraphicsOpenWindow,
                         stGraphicsOpenWindowFont,
                         stGraphicsOpenWindowFontName,
                         stGraphicsOpenWindowFontSize,
                         stGraphicsOpenWindowFontColor,
                         stGraphicsOpenWindowFontBold,
                         stGraphicsOpenWindowFontItalic,
                         stGraphicsOpenWindowFontUnderlined,
                         stGraphicsOpenWindowColors,
                         stGraphicsOpenWindowColorsCollectionBackgroundColor,
                         stGraphicsOpenWindowColorsCollectionTextColor,
                         //stGraphicsOpenWindowColorsCollectionHighlightBackgroundColor,
                         //stGraphicsOpenWindowColorsCollectionHighlightTextColor
                         stGraphicsOptionsWindow,
                         stGraphicsOptionsWindowFont,
                         stGraphicsOptionsWindowFontName,
                         stGraphicsOptionsWindowFontSize,
                         stGraphicsOptionsWindowFontColor,
                         stGraphicsOptionsWindowFontBold,
                         stGraphicsOptionsWindowFontItalic,
                         stGraphicsOptionsWindowFontUnderlined,
                         stGraphicsDuplicatesWindow,
                         stGraphicsDuplicatesWindowFont,
                         stGraphicsDuplicatesWindowFontName,
                         stGraphicsDuplicatesWindowFontSize,
                         stGraphicsDuplicatesWindowFontColor,
                         stGraphicsDuplicatesWindowFontBold,
                         stGraphicsDuplicatesWindowFontItalic,
                         stGraphicsDuplicatesWindowFontUnderlined,
                         stGraphicsHelpWindow,
                         stGraphicsHelpWindowFont,
                         stGraphicsHelpWindowFontName,
                         stGraphicsHelpWindowFontSize,
                         stGraphicsHelpWindowFontColor,
                         stGraphicsHelpWindowFontBold,
                         stGraphicsHelpWindowFontItalic,
                         stGraphicsHelpWindowFontUnderlined
                         );


  TSettingsBaseType    = (stNone,stInteger,stString,stFont);

  TSettingsEditor      = (seNone,
                          seBoolean,seInteger,sePct,seText,seSpin,
                          seColor,seMPlayerColor,seFont,
                          seSolution,seOptimization,
                          seSoundFileName,sePlayListItem,seImageFileName,
                          seFileTypeFilter,seMusicSource,seFilePath,
                          seImageRect,seImageView,seDrive,seAntiAliasing,
                          seMusicPlayListFileName,seActivity,seTitleMode,
                          seResizeToWindowWidth,
                          sePaletteFileName,
                          seOnEscapeKeyAction,
                          seOnLeftClickEmptySquareAction,
                          seOnRightClickAction,
                          seOnShiftKeyAction,
                          seCursorType,
                          seBoardDimensionsAsText,
                          seDeadlockDetectionType,
                          seDisplayDriverChangedAction,
                          seWallType,
                          seInBetweenFramesType,
                          seFloorFillCharacter,
                          seSolverFileName,
                          seOptimizerFileName,
                          sePluginThreadPriority,
                          seAlternatingOptimizations,
                          seAlternatingOptimizationsTrigger,
                          seAlternatingOptimizationsRepeatType,
                          seSkinExportFormatType
                         );

const
  SettingsEditorToSettingsBaseType
                       : array[TSettingsEditor] of TSettingsBaseType =
                         (stNone,
                          stInteger,stInteger,stInteger,stString,stInteger,
                          stInteger,stInteger,stFont,
                          stInteger,stInteger,
                          stString,stString,stString,
                          stString,stInteger,stString,
                          stInteger,stInteger,stString,stInteger,
                          stString,stInteger,stInteger,
                          stInteger,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString,
                          stString
                         );


  SettingsEditor       : array[TSettings] of TSettingsEditor =
    (
seNone,//Control
seNone,//	Move Animation
seBoolean,//            Enabled
seBoolean,//            Smooth animation
seBoolean,//            Player direction animation (appropriate images required)
seSpin,//		Do move (millisec.)
seSpin,//		Undo move (millisec.)
seSpin,//		Replay moves (millisecs. per move)
seBoolean,//            Mouse wheel up/down animates moves, if animation is enabled
seNone,//               Forklift driving (appropriate images required)
seBoolean,//                   Enabled
sePct,//                       Drive in reverse (squares) // (not really 'sePct' but 1-100 will suffice)
seNone,//       Pathfinding
seSpin,//               Max. search time (millisecs.)
seSpin,//               Max. pushes lower bound calculation time (millisecs.)
seOptimization,//       Minimize moves or pushes
seNone,//               Show legal moves
seBoolean,//                   Enabled
sePct,//                       Size (%)
seBoolean,//                   Transparent image
sePct,//                       Transparency (%)
seNone,//                      Player cursor
seBoolean,//                          Enabled
seColor,//                            Color
seSpin,//                             Pen width (pixels)
sePct,//                              Size (pixels) // (not really 'sePct' but 1-100 will suffice)
seNone,//                             Shadow
seBoolean,//                                  Enabled
seColor,//                                    Color
seNone,//                      Box cursor
seBoolean,//                          Enabled
seColor,//                            Color
seSpin,//                             Pen width (pixels)
sePct,//                              Size (pixels) // (not really 'sePct' but 1-100 will suffice)
seNone,//                             Shadow
seBoolean,//                                  Enabled
seColor,//                                    Color
seNone,//                      Deadlocks
seBoolean,//                          Enabled
seColor,//                            Color
seNone,//                      Jump moves
seBoolean,//                          Enabled
seNone,//                             Colors
seColor,//                                    Background
seColor,//                                    Text
seColor,//                                    Text shadow
seNone,//                      Animation (appropriate images required)
seBoolean,//                          Player animation enabled
seBoolean,//                          Box animation enabled
seBoolean, //                  Show board coordinates during gameplay
seNone,//       Solutions
seSolution,//           "Show solution" preferred type
seBoolean,//            Save best solutions automatically
seBoolean,//            Save old best solutions after finding better ones
seBoolean,//            Save old best solutions after clipboard import
seBoolean,//            Solutions require at least one push
seNone,//               Animation (appropriate images required)
seBoolean,//                    Player animation enabled
seBoolean,//                    Box animation enabled
{
seNone,//               Score metrics weights (smallest score is best)
seSpin,//                      Box lines
seSpin,//                      Box changes
seSpin,//                      Pushing sessions
}
seBoolean,//            Include secondary metrics in solution button hint
seBoolean,//            Allow deletion of best solutions in the "Snapshots" window
seNone,//               Error recovery
seBoolean,//                   Enabled
seNone,//       Snapshots
seBoolean,//            Save variations automatically
seBoolean,//            Include secondary metrics (e.g., box lines) in titles
seNone,//       Reverse mode
seBoolean,//            Show box startpositions as goals
seBoolean,//            Allow jumps after first box is moved
seNone,//       Do/Undo moves
seBoolean,//            Undo/redo combined moves in one operation
seBoolean,//	        "Move back" handled as "Undo move"
seOnLeftClickEmptySquareAction,
//                      Left-click empty square action
seOnRightClickAction,// Right-click action
seOnShiftKeyAction,//   Shift key action
seNone,//       Deadlock detection,
seBoolean,//            Enabled,
seDeadlockDetectionType,
//                      Type
seBoolean,//            Block moves leading to low and medium type deadlocks
seBoolean,//            Log deadlocks calculated at level load time (for testing)
seNone,//               Show simple deadlock squares
seBoolean,//                   Enabled
seColor,//                     Color
sePct,//                       Transparency (%)
seNone,//       Timing
seBoolean,//            Enabled
seNone,//               Idle time threshold
seBoolean,//                   Enabled
seSpin,//                      Seconds
seNone,//       Windows
seNone,//               Main window
seSpin,//                      Left
seSpin,//                      Top
seSpin,//                      Width
seSpin,//                      Height
seBoolean,//                   Board dimensions in title line
seBoolean,//                   Collection name in title line
seNone,//               Open window
seSpin,//                      Left
seSpin,//                      Top
seSpin,//                      Width
seSpin,//                      Height
seNone,//                      Disk group box
seSpin,//                               Width
seSpin,//                               Height
seNone,//                      Folder list box
seSpin,//                               Width
seNone,//                      Level preview
seSpin,//                               Height
seSpin,//                      Solution columns
seBoolean,//                   Include built-in solutions
seNone,//                      ToolTips
seBoolean,//                            Enabled
seSpin,//                               Horizontal offset, pixels
seSpin,//                               Vertical offset, pixels
seNone,//               Tools window
seSpin,//                      Left
seSpin,//                      Top
seSpin,//                      Width
seSpin,//                      Height
seBoolean,//                   Collection name in title line
seNone,//               Snapshots and solutions window
seSpin,//                      Left
seSpin,//                      Top
seSpin,//                      Width
seSpin,//                      Height
seNone,//               Internal clipboard window
seSpin,//                      Left
seSpin,//                      Top
seSpin,//                      Width
seSpin,//                      Height
seNone,//       Cursors
seCursorType,//         Selection cursor
seCursorType,//         Drag cursor
seCursorType,//         Tracking cursor
seNone,//       Tools
seNone,//               Solver
seSolverFileName,//            FileName
sePluginThreadPriority,//      Priority
seBoolean,//                   Add plugin name to solution titles
seBoolean,//                   Add date to solution notes
seBoolean,//                   Statistics enabled
seNone,//                      Time limit (host side as opposed to plugin side)
seBoolean,//                            Enabled
seSpin,//                               Seconds
seNone,//               Optimizer
seOptimizerFileName,//         FileName
sePluginThreadPriority,//      Priority
seBoolean,//                   Add plugin name to solution titles
seBoolean,//                   Add date to solution notes
seBoolean,//                   Statistics enabled
seBoolean,//                   Discard detroned solutions found in the same session
seBoolean,//                   Separate "best moves" and "best pushes" solutions: Initialize optimization accordingly
seNone,//                      Time limit (host side as opposed to plugin side)
seBoolean,//                            Enabled
seSpin,//                               Seconds
seNone,//                      Alternating optimizations
seBoolean,//                            Enabled
seAlternatingOptimizations,//           Type
seAlternatingOptimizationsTrigger, //   Trigger
seNone,//                               Repeat
seBoolean,//                                    Repeat
seAlternatingOptimizationsRepeatType,//         Type
seNone,//                      Intervals
seBoolean,//                            "Wall-ify" boxes outside range of pushes to be optimized
sePct,//                                Subinterval overlap (%)
seNone,//               Generator
sePluginThreadPriority,//      Priority
seNone,//               Level and skin capture
seBoolean,//                   Snap to nearby edges
seNone,//                      Grid
seColor,//                              Color
seColor,//                              Shadow color
seNone,//                      Texts
seText,//                               Default skin file name
seNone,//                      Append new levels to collections
seBoolean,//                            Check for duplicates (disable on your own risk of overwriting existing levels)
seNone,//               Duplicate finder
seBoolean,//                   Advanced identity check (misses some symmetric duplicates)
seNone,//                      Current level
sePct,//                                Match threshold (%)
seNone,//                      Current collection
seNone,//                               (No options)
seNone,//                      All levels
seNone,//                               (No options)
seNone,//               Skin export
seSkinExportFormatType,//      Base format, columns x rows
seNone,//        Level format
seBoolean,//		Add file format description to level files
seBoolean,//		Pretty-print saved solutions and snapshots
seNone,//               Copy to clipboard
seBoolean,//                   Fill floors
seFloorFillCharacter,//        Floor fill character
seBoolean,//                   Level collections: Include level comments
seBoolean,//                   Show combined moves
seBoolean,//                   Export level based on solution slice: Include titles
seFloorFillCharacter,// Run-length encoding: Floor character
seNone,//       Miscellaneous
//seBoolean,//          Associate ".sok" files and ".xsb" files with this application
seBoolean,//            Restore last position when a level is reopened
seBoolean,//            Auto advance to next level when a level has been solved
seBoolean,//            Mouse tracking mode enabled
seBoardDimensionsAsText,
//                      Board dimensions
seBoolean,//            Board dimensions: Show floor count
seOnEscapeKeyAction,//  On pressing the [Esc] key in the main window
seDisplayDriverChangedAction,
//                      On changing Windows' display driver
seBoolean,//            Confirm running multiple instances of the application
seBoolean,//            Enable screensaver and "monitor power off"
seBoolean,//            Use only one processor (changes will not take effect until next time the application is launched)
seNone,//Sound
seBoolean,//	Sound enabled
sePct,//        Sound volume
seBoolean,//    Reset sound effects when a skin is loaded
seNone,//	Sound effects
seNone,//		Move
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Push
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Goal
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Block
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Solution
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Undo move
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Jump
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Restart
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Menu button
seBoolean,//			Enabled
seSoundFileName,//		Filename
seNone,//		Menu selection
seBoolean,//			Enabled
seSoundFileName,//		Filename
//seFileTypeFilter,//	Sound Filetypes

{$IFDEF MUSIC_PLAYER}
seNone,//Music
seBoolean,//	Music enabled
seMusicSource,//Music source
seDrive,//      CD Drive
seFilePath,//	Music folder
seMusicPlayListFileName,//	PlaylistFileName
seFileTypeFilter,//Music Filetypes
seNone,//       MusicPlayer
seBoolean,//            Transparency
sePct,//                TransparencyPct
seMPlayerColor,//       Color
seNone,//               Display
sePct,//                        DisplayTransparencyPct
seNone,//	                Font
seFont,//	     	                Name
seFont,//	     	                Size
seColor,//	     	                Color
seBoolean,//	     	                Bold
seBoolean,//	     	                Italic
seBoolean,//	     	                Underlined
seTitleMode,//                  Track Title
seBoolean,//                    Show Frames/Second
seNone,//                       Activities
seActivity,//                           Activity
seNone,//                               Activities
seNone,//                                      Mandala
seNone,//                                          Image Engine
seNone,//                                          Color Cycling
sePct,// (not really 'Pct', but 0..100 will suffice)       Minimum Speed (Steps per Second)
sePct,// (not really 'Pct', but 0..100 will suffice)       Maximum Speed (Steps per Second)
seSpin,//                                                  Interval for Changing Speed (milliseconds)
sePct,//                                                   Deviation (%)
seNone,//                                              Color Change
sePct,// (not really 'Pct', but 0..100 will suffice)       Minimum Speed (Steps per Second)
sePct,// (not really 'Pct', but 0..100 will suffice)       Maximum Speed (Steps per Second)
seSpin,//                                                  Interval for Changing Speed (milliseconds)
sePct,//                                                   Deviation (%)
seNone,//                                              Color Effects
seSpin,//                                                  Interval for Changing Effect (milliseconds)
sePct,//                                                   Deviation (%)
seNone,//                                              Patterns
seSpin,//                                                  Interval for Changing Pattern (milliseconds)
sePct,//                                                   Deviation (%)
seNone,//                                                      Fade
sePct,//                                                           Steps
sePct,// (not really 'Pct', but 0..100 will suffice)               Step Interval (milliseconds)
seNone,//                                                      Morph
sePct,// (not really 'Pct', but 0..100 will suffice)               Step Interval (milliseconds)
seBoolean,//                                           Show Title on Startup
seNone,//                                          Menu
sePct,//                                               Transparency (%)
seBoolean,//                                           Text Shadow
seNone,//                                              Colors
seColor,//                                                 Background
seColor,//                                                 Button Text
seColor,//                                                 Focused Button Text
seColor,//                                                 Grayed Button Text
seColor,//                                                 Text Shadow
seNone,//                                              Font
seFont,//                                                  Name
seFont,//                                                  Size
seColor,//                                                 Color
seBoolean,//                                               Bold
seBoolean,//                                              Italic
seBoolean,//                                              Underlined
seNone,//                                      Image Viewer
seImageFileName,//                                 FileName
seResizeToWindowWidth,//                           View
seNone,//                                          Slide show
seBoolean,//                                           Enabled
seSpin,//                                              Interval (milliseconds)
seSpin,//                                              Fade (milliseconds)
seBoolean,//                                           Random Order
seNone,//                                          Menu
sePct,//                                               Transparency (%)
seBoolean,//                                           Text Shadow
seNone,//                                              Colors
seColor,//                                                 Background
seColor,//                                                 Button Text
seColor,//                                                 Focused Button Text
seColor,//                                                 Grayed Button Text
seColor,//                                                 Text Shadow
seNone,//                                              Font
seFont,//                                                  Name
seFont,//                                                  Size
seColor,//                                                 Color
seBoolean,//                                                Bold
seBoolean,//                                               Italic
seBoolean,//                                               Underlined
{
seNone,//                                      Fireworks
seNone,//                                          1-Color Rockets
seSpin,//                                              Interval (milliseconds)
sePct,//                                               Deviation (%)
seNone,//                                          2-Color Rockets
seSpin,//                                              Interval (milliseconds)
sePct,//                                               Deviation (%)
seNone,//                                          Menu
sePct,//                                               Transparency (%)
seBoolean,//                                           Text Shadow
seNone,//                                              Colors
seColor,//                                                 Background
seColor,//                                                 Button Text
seColor,//                                                 Focused Button Text
seColor,//                                                 Grayed Button Text
seColor,//                                                 Text Shadow
seNone,//                                              Font
seFont,//                                                  Name
seFont,//                                                  Size
seColor,//                                                 Color
seBoolean,//                                               Bold
seBoolean,//                                               Italic
seBoolean,//                                               Underlined
}
seNone,//                                      Fractals
sePaletteFileName,//                               Palette Filename
seBoolean,//                                       Antialiasing
seNone,//                                          Menu
sePct,//                                               Transparency (%)
seBoolean,//                                           Text Shadow
seNone,//                                              Colors
seColor,//                                                 Background
seColor,//                                                 Button Text
seColor,//                                                 Focused Button Text
seColor,//                                                 Grayed Button Text
seColor,//                                                 Text Shadow
seNone,//                                              Font
seFont,//                                                  Name
seFont,//                                                  Size
seColor,//                                                 Color
seBoolean,//                                               Bold
seBoolean,//                                               Italic
seBoolean,//                                               Underlined
seNone,//                               Menu
sePct,//                                    Transparency (%)
seBoolean,//                                Text Shadow
seNone,//                                   Colors
seColor,//                                      Background
seColor,//                                      Button Text
seColor,//                                      Focused Button Text
seColor,//                                      Grayed Button Text
seColor,//                                      Text Shadow
seNone,//                                   Font
seFont,//                                       Name
seFont,//                                       Size
seColor,//                                      Color
seBoolean,//                                    Bold
seBoolean,//                                    Italic
seBoolean,//                                    Underlined
seNone,//	        Open window
seNone,//			Colors
seColor,//                              Background
seColor,//                              BackgroundText
seColor,//                              Highlighted Text
seColor,//                              Button
seColor,//                              Button Text
seColor,//                              Focused Button
seColor,//                              Focused Button Text
seColor,//                              Grayed Button
seColor,//                              Grayed Button Text
seColor,//                              Window
seColor,//                              Window Text
seColor,//                              Focused Window
seColor,//                              Focused Window Text
{$ENDIF}
seNone,//Graphics
seNone,//	Background
seBoolean,//		Visible
seNone,//		Image
seImageFileName,//  		Filename
seNone,//			Section
seImageRect,//				Left
seImageRect,//				Top
seImageRect,//				Right
seImageRect,//				Bottom
seImageView,//			View
seAntiAliasing,//               Antialiasing
seColor,//		Color
seNone,//	Board
seNone,//		Background
seBoolean,//			Visible
seNone,//			Image
seImageFileName,//			Filename
seNone,//				Section
seImageRect,//					Left
seImageRect,//					Top
seImageRect,//					Right
seImageRect,//					Bottom
seImageView,//			        View
seAntiAliasing,//                       Antialiasing
seColor,//                              Background color
sePct,//                                Background color tolerance (%)
seBoolean,//                            Mask image using background color
seBoolean,//                            Transparency
sePct,//                                Transparency (%)
seColor,//			Color
seNone,//		Reverse mode background
seBoolean,//			Visible
seNone,//			Image
seImageFileName,//			Filename
seNone,//				Section
seImageRect,//					Left
seImageRect,//					Top
seImageRect,//					Right
seImageRect,//					Bottom
seImageView,//			        View
seAntiAliasing,//                       Antialiasing
seColor,//                              Background color
sePct,//                                Background color tolerance (%)
seBoolean,//                            Mask image using background color
seBoolean,//                            Transparency
sePct,//                                Transparency (%)
seColor,//			Color
seNone, //                      Player start position
seBoolean,//                            Visible
seSpin,//                               Grid cell size, pixels
seColor,//                              Grid color
seColor,//                              Grid shadow color
seNone,//               Figures
seNone,//		        Player
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seColor,//			        Color
seNone,//                               Highlight animation
seNone,//		         	        Image
seImageFileName,//		        	       Filename
seNone,//				               Section
seImageRect,//					              Left
seImageRect,//					              Top
seImageRect,//				                      Right
seImageRect,//					              Bottom
seBoolean,//                                           Mask image using background color
seColor,//				               Background color
sePct,//				               Background color tolerance (%)
seBoolean,//                                           Background color masks exterior pixels only
seAntiAliasing,//                                      Antialiasing
seInBetweenFramesType,//                        In-between frames
seNone,//		        Player on goal square
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seColor,//			        Color
seBoolean,//                            Use "Player" image for move animation
seBoolean,//                            Use "Player" image for highlighting reachable squares
seNone,//                               Highlight animation
seNone,//		         	        Image
seImageFileName,//		        	       Filename
seNone,//				               Section
seImageRect,//					              Left
seImageRect,//					              Top
seImageRect,//				                      Right
seImageRect,//					              Bottom
seBoolean,//                                           Mask image using background color
seColor,//				               Background color
sePct,//				               Background color tolerance (%)
seBoolean,//                                           Background color masks exterior pixels only
seAntiAliasing,//                                      Antialiasing
seInBetweenFramesType,//                        In-between frames
seNone,//		        Box
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seColor,//			        Color
seNone,//                               Highlight animation
seNone,//		         	        Image
seImageFileName,//		        	       Filename
seNone,//				               Section
seImageRect,//					              Left
seImageRect,//					              Top
seImageRect,//				                      Right
seImageRect,//					              Bottom
seBoolean,//                                           Mask image using background color
seColor,//				               Background color
sePct,//				               Background color tolerance (%)
seBoolean,//                                           Background color masks exterior pixels only
seAntiAliasing,//                                      Antialiasing
seInBetweenFramesType,//                        In-between frames
seNone,//		        Box on goal square
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seColor,//			        Color
seBoolean,//                            Use "Box" image for move animation
seBoolean,//                            Use "Box" image for highlighting reachable squares
seNone,//                               Highlight animation
seNone,//		         	        Image
seImageFileName,//		        	       Filename
seNone,//				               Section
seImageRect,//					              Left
seImageRect,//					              Top
seImageRect,//				                      Right
seImageRect,//					              Bottom
seBoolean,//                                           Mask image using background color
seColor,//				               Background color
sePct,//				               Background color tolerance (%)
seBoolean,//                                           Background color masks exterior pixels only
seAntiAliasing,//                                      Antialiasing
seInBetweenFramesType,//                        In-between frames
seNone,//		        Goal
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seBoolean,//                                    Transparency
sePct,//                                        Transparency (%)
seColor,//			        Color
senone,//		        Wall
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seColor,//			        Color
seWallType,//                           Type (appropriate images required)
seNone,//                               Wall cap offset, pixels
seSpin,//                                       Left  -1..100 // kludge: '-1' = offset 0; '0' = default offset = midways between the squares
seSpin,//                                       Top   -1..100
seNone,//                               Outer wall razor, pixels // seamless walls only
sePct,//                                        Left // measured in pixels, not pct, but 1..100 will do
sePct,//                                        Top
sePct,//                                        Right
sePct,//                                        Bottom
senone,//		        Floor
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seBoolean,//                                    Mask image using background color
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//                                    Background color masks exterior pixels only
seAntiAliasing,//                               Antialiasing
seBoolean,//                                    Transparency
sePct,//                                        Transparency (%)
seColor,//			        Color
seBoolean,//                            Visible
seSpin,//                        Maximum zoom factor (%)
seNone, //              Grid (appropriate images required)
seBoolean,//                     Enabled
seColor,//                       Color 1
seColor,//                       Color 2
seNone,//	Menu
seBoolean,//		Transparent buttons
seBoolean,//            Text shadow
seNone,//		Buttons
seNone,//			Disabled button
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//				Mask image using background color
sePct,//				Transparency (%)
seColor,//				Font color
seColor,//                              Text shadow color
seNone,//			Enabled button
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//				Mask image using background color
sePct,//				Transparency (%)
seColor,//				Font color
seColor,//                              Text shadow color
seNone,//			Focused, enabled button
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//				Mask image using background color
sePct,//				Transparency (%)
seColor,//				Font color
seColor,//                              Text shadow color
seNone,//			Focused, disabled button
seNone,//			        Image
seImageFileName,//			        Filename
seNone,//				        Section
seImageRect,//					       Left
seImageRect,//					       Top
seImageRect,//					       Right
seImageRect,//					       Bottom
seColor,//				        Background color
sePct,//				        Background color tolerance (%)
seBoolean,//				Mask image using background color
sePct,//				Transparency (%)
seColor,//				Font color
seColor,//                              Text shadow color
seNone,//		Edges
seBoolean,//			Edgesmoothing enabled
sePct,//			Transparency (%)
seNone,//		Font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//			Underlined
seNone,//               Popup menu
seNone,//                       Colors
seColor,//                              Background
seColor,//                              BackgroundText
seColor,//                              Highlighted Text
seColor,//                              Button
seColor,//                              Button Text
seColor,//                              Focused Button
seColor,//                              Focused Button Text
seColor,//                              Grayed Button
seColor,//                              Grayed Button Text
seColor,//                              Window
seColor,//                              Window Text
seColor,//                              Focused Window
seColor,//                              Focused Window Text
seNone,//	Status
seNone,//		Image
seImageFileName,//		Filename
seNone,//			Section
seImageRect,//				Left
seImageRect,//				Top
seImageRect,//				Right
seImageRect,//				Bottom
seColor,//			Background color
sePct,//			Background color tolerance (%)
seBoolean,//		Mask image using background color
sePct,//		Transparency (%)
seNone,//		Edges
seBoolean,//			Edgesmoothing enabled
sePct,//			Transparency (%)
seNone,//		Image Slices
seSpin,//			Left
seSpin,//			Body
seSpin,//			Right
seNone,//		Buttons
sePct,//			Transparency (%)
seNone,//		Panels
seNone,//			Bounds
seSpin,//				Top
seSpin,//				Bottom
seColor,//                      Color
sePct,//			Transparency (%)
sePct,//			Rounding rectangle (pixels) (not really 'Pct', but 0..100 will suffice)
seNone,//		Font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//			Underlined
seNone,//               Subtexts
seBoolean,//                    Transparent subtexts
seColor,//                      Font color
seSpin,//                       Font Size
seBoolean,//            Text shadow
seColor,//              Text shadow color
seNone,//       Multiple views
seNone,//               Colors
seColor,//                      Background
seColor,//                      Focused background
seColor,//                      Line
seColor,//                      Text
seColor,//                      Focused text
seColor,//                      Shadow
sePct,//                Background transparency (%)
seNone,//		Font
seFont,//		        Name
seFont,//		        Size
seColor,//		        Color
seBoolean,//		        Bold
seBoolean,//		        Italic
seBoolean,//		        Underlined
seNone,//       Tools
seNone,//		Window font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//			Underlined
seNone,//               Buttons
seNone,//                       Image
seImageFileName,//                      Filename
seNone,//               Board
seNone,//	                Background
seBoolean,//		                Visible
seNone,//	                        Image
seImageFileName,//  		               Filename
seImageView,//			               View
seAntiAliasing,//                              Antialiasing
seColor,//                              Color
seNone,//                       Skin
seNone,//                               Image
seImageFileName,//                             Filename
seBoolean,//                                   Mask image using background color
seColor,//                                     Background color
sePct,//                                       Background color tolerance (%)
seAntiAliasing,//                              Antialiasing
seNone,//                       Square set
seColor,//                              Selected squares color
seColor,//                              Not selected squares color
sePct,//                                Transparency (%)
seNone,//               Editor
seNone,//                       Cursors
seNone,//                               Cell cursor
seColor,//                                     Pen color
seSpin,//                                      Pen width (pixels)
seColor,//                                     Shadow color
seNone,//                               Eraser cursor
seColor,//                                     Pen color
seSpin,//                                      Pen width (pixels)
seColor,//                                     Shadow color
seNone,//                               Selection cursor
seColor,//                                     Pen color
seSpin,//                                      Pen width (pixels)
seColor,//                                     Shadow color
seNone,//                       Frame
seColor,//                              Color
seColor,//                              Shadow color
seNone,//                       Grid
seBoolean,//                            Enabled
seNone,//               Replay
seNone,//                       Speed track bar colors
seColor,//                              Background color
seColor,//                              Slider color
seColor,//                              Font color
seColor,//                              Shadow color
seNone,//               Internal clipboard
seNone,//                       Window font
seFont,//			        Name
seFont,//			        Size
seColor,//			        Color
seBoolean,//			        Bold
seBoolean,//			        Italic
seBoolean,//			        Underlined
seNone,//	                Colors
seColor,//                              Background
seColor,//                              BackgroundText
seColor,//                              Highlighted Text
seColor,//                              Button
seColor,//                              Button Text
seColor,//                              Focused Button
seColor,//                              Focused Button Text
seColor,//                              Grayed Button
seColor,//                              Grayed Button Text
seColor,//                              Window
seColor,//                              Window Text
seColor,//                              Focused Window
seColor,//                              Focused Window Text
seColor,//                              Notes - window
seNone,//		        Edit font
seFont,//			        Name
seFont,//			        Size
seColor,//			        Color
seBoolean,//			        Bold
seBoolean,//			        Italic
seBoolean,//			        Underlined
seNone,//                       Texts
seText,//                               Clipboard item
seNone,//               Duplicate levels
seNone,//                       Colors
seColor,//                              Background color 1
seColor,//                              Text color 1
seColor,//                              Background color 2
seColor,//                              Text color 2
seColor,//                              Highlight background color
seColor,//                              Highlight text color
seNone,//       Snapshots and solutions
seNone,//		Window font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//			Underlined
seNone,//	        Colors
seColor,//                      Background
seColor,//                      BackgroundText
seColor,//                      Highlighted Text
seColor,//                      Button
seColor,//                      Button Text
seColor,//                      Focused Button
seColor,//                      Focused Button Text
seColor,//                      Grayed Button
seColor,//                      Grayed Button Text
seColor,//                      Window
seColor,//                      Window Text
seColor,//                      Focused Window
seColor,//                      Focused Window Text
seColor,//                      Notes - window
seNone,//		Edit font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//			Underlined
seNone,//               Texts
seText,//                       Solution
seText,//                       Best Solution
seText,//                       Solution/Moves
seText,//                       Solution/Pushes
seText,//                       Normal mode game
seText,//                       Reverse mode game
seNone,//       Open window
seNone,//		Window font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//			Underlined
seNone,//               Colors
seColor,//                      Collection background color
seColor,//                      Collection text color
//seColor,//                    Collection highlight background color
//seColor//                     Collection highlight text color
seNone,//       Options window
seNone,//		Window font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//                    Underlined
seNone,//       Duplicates window
seNone,//		Window font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean,//                    Underlined
seNone,//       Help window
seNone,//		Window font
seFont,//			Name
seFont,//			Size
seColor,//			Color
seBoolean,//			Bold
seBoolean,//			Italic
seBoolean//                     Underlined
);

const
  PictureSettingsOffset       : array[GView_.TPictureType] of Integer =
                                  (Ord(stGraphicsBackground),
                                   Ord(stGraphicsBoardBackground),
                                   Ord(stGraphicsBoardReverseModeBackground),
                                   Ord(stGraphicsBoardFiguresPlayer),
                                   Ord(stGraphicsBoardFiguresPlayerOnGoal),
                                   Ord(stGraphicsBoardFiguresBox),
                                   Ord(stGraphicsBoardFiguresBoxOnGoal),
                                   Ord(stGraphicsBoardFiguresGoal),
                                   Ord(stGraphicsBoardFiguresWall),
                                   Ord(stGraphicsBoardFiguresFloor),
                                   Ord(stGraphicsBoardFiguresPlayerAnimation),
                                   Ord(stGraphicsBoardFiguresPlayerOnGoalAnimation),
                                   Ord(stGraphicsBoardFiguresBoxAnimation),
                                   Ord(stGraphicsBoardFiguresBoxOnGoalAnimation),
                                   Ord(stGraphicsBoardFiguresPlayer),
                                   Ord(stGraphicsBoardFiguresPlayerOnGoal),
                                   Ord(stGraphicsBoardFiguresBox),
                                   Ord(stGraphicsBoardFiguresBoxOnGoal)
                                  );

  PictureSettingsColorIndex   : array[GView_.TPictureType] of Integer =
                                  (Ord(stGraphicsBackgroundColor),
                                   Ord(stGraphicsBoardBackgroundColor),
                                   Ord(stGraphicsBoardReverseModeBackgroundColor),
                                   Ord(stGraphicsBoardFiguresPlayerColor),
                                   Ord(stGraphicsBoardFiguresPlayerOnGoalColor),
                                   Ord(stGraphicsBoardFiguresBoxColor),
                                   Ord(stGraphicsBoardFiguresBoxOnGoalColor),
                                   Ord(stGraphicsBoardFiguresGoalColor),
                                   Ord(stGraphicsBoardFiguresWallColor),
                                   Ord(stGraphicsBoardFiguresFloorColor),
                                   Ord(stGraphicsBoardFiguresPlayerColor),
                                   Ord(stGraphicsBoardFiguresPlayerOnGoalColor),
                                   Ord(stGraphicsBoardFiguresBoxColor),
                                   Ord(stGraphicsBoardFiguresBoxOnGoalColor),
                                   Ord(stGraphicsBoardFiguresPlayerColor),
                                   Ord(stGraphicsBoardFiguresPlayerOnGoalColor),
                                   Ord(stGraphicsBoardFiguresBoxColor),
                                   Ord(stGraphicsBoardFiguresBoxOnGoalColor)
                                  );


type
  TOptionsForm = class(TForm)
    BottomPanel: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    DefaultBtn: TButton;
    StatusBar1: TStatusBar;
    TreeView1: TTreeView;
    TopPanel: TPanel;
    StringGrid1: TStringGrid;
    MainMenu1: TMainMenu;
    MenuItemExit: TMenuItem;
    MenuItemFile: TMenuItem;
    N1: TMenuItem;
    MenuItemSaveTree: TMenuItem;
    Panel1: TPanel;
    SpinEdit1: TSpinEdit;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    N2: TMenuItem;
    MenuItemSaveAsFile: TMenuItem;
    MenuItemOpenFile: TMenuItem;
    N3: TMenuItem;
    MenuItemDeleteFiles: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    SaveDialog1: TSaveDialog;
    MenuItemHelp: TMenuItem;
    MenuItemTopMenuExit: TMenuItem;
    MenuItemView: TMenuItem;
    MenuItemCollapseTree: TMenuItem;
    MenuItemExpandTree: TMenuItem;
    MenuItemSkins: TMenuItem;
    MenuItemDefaultSkin: TMenuItem;
    MenuItemRecentSkinsSeparator: TMenuItem;
    MenuItemLoadSkin: TMenuItem;
    MenuItemRecentSkins: TMenuItem;
    N5: TMenuItem;
    MenuItemClearRecentSkins: TMenuItem;
    MenuItemButtons: TMenuItem;
    MenuItemBuiltinBlueButtons: TMenuItem;
    MenuItemBuiltinGreenButtons: TMenuItem;
    MenuItemBuiltinOrangeButtons: TMenuItem;
    SelectBtn: TButton;
    MenuItemBuiltinRedButtons: TMenuItem;
    MenuItemDefaultSkinRedBoxes: TMenuItem;
    MenuItemDefaultSkinYellowBoxes: TMenuItem;
    ApplyBtn: TButton;
    MenuItemEdit: TMenuItem;
    MenuItemCopyToClipboard: TMenuItem;
    MenuItemPasteFromClipboardFull: TMenuItem;
    N4: TMenuItem;
    MenuItemDefaultSkinSeamlessWalls: TMenuItem;
    MenuItemDefaultSkinTiledWalls: TMenuItem;
    MenuItemDefaultSkinBrickWalls: TMenuItem;
    MenuItemDefaultSkinYellowCrates: TMenuItem;
    MenuItemPasteFromClipboardSupplementary: TMenuItem;
    N6: TMenuItem;
    MenuItemExportSkin: TMenuItem;
    N7: TMenuItem;
    MenuItemDefaultSkinDefaultBackgroundAndButtons: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormResize(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure DefaultBtnClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Expanded(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Click(Sender: TObject);
    procedure TreeView1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MenuItemExitClick(Sender: TObject);
    procedure MenuItemSaveTreeClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      ARect: TRect; AState: TGridDrawState);
    procedure StringGrid1TopLeftChanged(Sender: TObject);
    procedure StringGrid1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel1Click(Sender: TObject);
    procedure EditorExit(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ComboBox1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpinEdit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpinEdit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormDeactivate(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure Edit1Change(Sender: TObject);
    procedure MenuItemOpenFileClick(Sender: TObject);
    procedure MenuItemSaveAsFileClick(Sender: TObject);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView;
      Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure MenuItemDeleteFilesClick(Sender: TObject);
    procedure MenuItemViewClick(Sender: TObject);
    procedure MenuItemCollapseTreeClick(Sender: TObject);
    procedure MenuItemExpandTreeClick(Sender: TObject);
    procedure MenuItemSkinsClick(Sender: TObject);
    procedure MenuItemClearRecentSkinsClick__(Sender: TObject);
    procedure MenuItemLoadSkinClick(Sender: TObject);
    procedure MenuItemDefaultSkinClick(Sender: TObject);
    procedure MenuItemLoadSkinFromHistoryClick(Sender: TObject);
    procedure MenuItemBuiltinButtonsClick(Sender: TObject);
    procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MenuItemCopyToClipboardClick(Sender: TObject);
    procedure MenuItemPasteFromClipboardFullClick(Sender: TObject);
    procedure MenuItemEditClick(Sender: TObject);
    procedure MenuItemDefaultSkinWallTypeClick(Sender: TObject);
    procedure MenuItemTopMenuExitClick(Sender: TObject);
    procedure MenuItemExportSkinClick(Sender: TObject);
    procedure MenuItemDefaultSkinDefaultBackgroundAndButtonsClick(
      Sender: TObject);
  private
    { Private declarations }
    BoardGraphicsModified:Boolean;
    DragPoint:TPoint;
    EditorRow:Integer;
    EscapeEnabled:Boolean;
    fMostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified:String;
    FontColorBranchNode:TColor;
    FontColorLeafNode:TColor;
    FTreeViewWndProc: TWndMethod;
    Initialized:Boolean;
    IsBusy:Boolean;
    IsResizing:Boolean;
    Modified:Boolean;
//  OldHasFileAssociations:Boolean;
    oSelected:TTreeNode;
    oTopItem:TTreeNode;
    FSettingsString:array[TSettings] of String;
    ScratchFont:TFont;
    SettingsFilterIndex:Integer;
    SettingsDirectory:String;
    SkinExportFormatType:TSkinExportFormatType;
    TreeView1LineCount:Integer;
    procedure ApplicationOnActivate(Sender: TObject);
    function  CommitOrDropChanges:Boolean;
    function  CloseEditors(HideFirst:Boolean):Boolean;
    function  ExternalToInternalFileTypeFilter(const s: String): String;
    function  LoadFromFile(const FileName:String; LoadDefaultValues__:Boolean):Boolean;
    function  GetFontSettings(Index: Integer; var FontIndex:Integer; var Font__: TFont): Boolean;
    function  GetSettingsHint(No:TSettings):String;
    function  GetSettingsString(No:TSettings):String;
    function  HasDefaultSkinWall:Boolean;
    function  HasDefaultSkinBrickWall:Boolean;
    function  HideEditors:Boolean;
    function  InternalToExternalFileTypeFilter(const s: String): String;
    function  IsASettingsFile(const FileName: String): Boolean;
    function  IsOpenFormAvailable:Boolean;
    function  RowToSettings(Row:Integer):TSettings;
    function  SaveToFile(const FileName:String; AllItems__:Boolean):Boolean;
    procedure SetEditColor;
    procedure SetFontSettings(FontIndex:Integer; Font__:TFont);
    procedure ShowEditor(Row:Integer);
    procedure ShowHint(Sender: TObject);
    procedure TreeToGrid;
    procedure SetSettingsString(No:TSettings; const s:String);
    property  SettingsHint[No:TSettings]:String read GetSettingsHint;
    procedure SetToDefaultValue(Node: TTreeNode);
    procedure TreeViewWndProc(var Msg: TMessage);
  protected
    function  GetPictureTypeFileName(PictureType__:GView_.TPictureType):String;
    function  GetPictureTypeFileNameIndex(PictureType__:GView_.TPictureType):TSettings;
    function  GetPictureTypeSection(PictureType__:GView_.TPictureType):TRect;
    procedure SetPictureTypeFileName(PictureType__:GView_.TPictureType; const FileName__:String);
    procedure SetPictureTypeSection(PictureType__:GView_.TPictureType; const Rect__:TRect);
  public
    { Public declarations }
    DefaultSettingsEnabled:array[TSettings] of Boolean;
    DefaultSettingsString :array[TSettings] of String;

    function  FullItemName(Node: TTreeNode): String;
    function  IsAFileNameSetting(TreeNode__:TTreeNode):Boolean;
    function  IsAPictureSetting(Index__:Integer; var PictureType__:TPictureType):Boolean;
    procedure LoadData;
    procedure LoadDefaultSkin(DefaultSkinWallType__:TDefaultSkinWallType; ResetSoundEffectsOnLoadingASkin__:Boolean);
    procedure LoadDefaultValues;
    function  LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
    function  MakeSkin(var {o:} SkinBitMap__ : TBitMap; var {o:} ColCount__, RowCount__ : Integer ): Boolean;
    procedure OnFontChange;
    procedure SaveAsDefaultValues;
    procedure SaveData( UpdateWindowSizes__ : Boolean );
    function  SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
    procedure SetColorTheme( ColorTheme__ : TColorTheme );
    procedure SetDefaultSkinMenuItemCheckMark(Sender: TObject);
    function  SettingsFileID:String;
    function  SettingsPath:String; // path without trailing path delimiter
    function  ShowDefaultSkinSubMenu:Boolean;
    procedure ShowStatus;
    function  UpdateSettings(ARow:Integer; s:String; Sender:TObject):Boolean;

    property  MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified : String read fMostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified write fMostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified;
    property  PictureTypeFileName[PictureType__:GView_.TPictureType]:String read GetPictureTypeFileName write SetPictureTypeFileName;
    property  PictureTypeFileNameIndex[PictureType__:GView_.TPictureType]:TSettings read GetPictureTypeFileNameIndex;
    property  PictureTypeSection [PictureType__:GView_.TPictureType]:TRect  read GetPictureTypeSection  write SetPictureTypeSection;
    property  SettingsString[No:TSettings]:String read GetSettingsString write SetSettingsString;
  end;

var
  OptionsForm: TOptionsForm = nil;

implementation

{$R *.DFM}

uses Clipbrd,
     SokUtil_,SokFile_,SokGame_,
     Open1_, Main_,Pict_,Menu_,Status_,Sound_,Music_,
     BitMap_,Display_,MPlayer2_,Res_,
     Mandal1_,IView1_,Fractal_,Fworks_,Game_,Snapshots_,Skins_, LSView_,
     Duplicates_,Tools_,Plugin_,Generator_, Capture_, Help_;

const
  DEFAULT_COLOR_TREE_VIEW                      = COLOR_LEMONCHIFFON;
  DEFAULT_COLOR_BRANCH_NODE                    = clBlue;
  DEFAULT_COLOR_LEAF_NODE                      = clGreen;

  DEFAULT_SKIN_EXPORT_FORMAT_TYPE              = seft4x4;

  DEFAULT_SKIN_VIOLET_PLAYER_LEFT              = 52;
  DEFAULT_SKIN_VIOLET_PLAYER_TOP               = 1;
  DEFAULT_SKIN_YELLOW_CRATE_LEFT               = 307;
  DEFAULT_SKIN_YELLOW_CRATE_TOP                = 52;
  DEFAULT_SKIN_YELLOW_CRATE_ON_GOAL_LEFT       = 307;
  DEFAULT_SKIN_YELLOW_CRATE_ON_GOAL_TOP        = 103;
  DEFAULT_SKIN_BRICK_WALL_LEFT                 = 205;
  DEFAULT_SKIN_BRICK_WALL_TOP                  = 154;

  OPTIONS_FORM_INIFILE_SECTION                 ='OptionsForm'; // don't localize
  TOP_PANEL_FONT_SIZE                          =12;

procedure TOptionsForm.FormCreate(Sender: TObject);
var i,j:Integer; Rect:TRect;
begin {$R+}
  OnFontChange;
  if biMinimize in BorderIcons then BorderIcons:=BorderIcons-[biMinimize]; // minimize: it may be impossible for the user to bring the application back on the screen again (a Microsoft Windows operating system bug when the general Windows text size settings is > 100%)
  Width:=DEFAULT_FORM_WIDTH; Height:=DEFAULT_FORM_HEIGHT;
  TopPanel.Font.Name:=Self.Font.Name;
  TopPanel.Font.Size:=TOP_PANEL_FONT_SIZE;
  Caption:=Application.Title+SUB_TITLE_SEPARATOR+Caption;
  OpenDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+OpenDialog1.Title;
  OpenDialog2.Title:=Application.Title+SUB_TITLE_SEPARATOR+OpenDialog2.Title;
  SaveDialog1.Title:=Application.Title+SUB_TITLE_SEPARATOR+SaveDialog1.Title;

  EscapeEnabled:=True; Initialized:=True; IsResizing:=False;
  FontColorLeafNode:=DEFAULT_COLOR_LEAF_NODE;
  FontColorBranchNode:=DEFAULT_COLOR_BRANCH_NODE;
  SkinExportFormatType:=DEFAULT_SKIN_EXPORT_FORMAT_TYPE;
  MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified:='';

  FTreeViewWndProc := TreeView1.WindowProc;
  TreeView1.WindowProc := TreeViewWndProc;

  with TreeView1 do begin
    Font.Size:=10;
    Font.Color:=clBlack;
    Color:=DEFAULT_COLOR_TREE_VIEW;
    Items[0].Expand(True); // to position 'StringGrid1' correctly
    end;

  with StringGrid1 do begin
    Font.Assign(TreeView1.Font);
    Color:=TreeView1.Color;
    Width:=150;
    Rect:=TreeView1.Items[0].DisplayRect(False);
    DefaultRowHeight:=Max(1,Rect.Bottom-Rect.Top)-GridLineWidth;
    try    RowCount:=Succ(Ord(High(TSettings))); // the only reason that 'StringGrid1' has more lines than 'VisibleRowCount' is, that mouse-wheel scrolling is handled automatically this way
    except on E:Exception do;
    end;
    end;

  try    ScratchFont:=TFont.Create;
  except on E:Exception do ScratchFont:=nil;
  end;

//Panel1   .Height:=StringGrid1.DefaultRowHeight-2*StringGrid1.GridLineWidth;
//Panel1   .Width :=Panel1.Height;
  SelectBtn.Height:=StringGrid1.DefaultRowHeight-2*StringGrid1.GridLineWidth;
  SelectBtn.Width :=SelectBtn.Height;
  Edit1    .Height:=StringGrid1.DefaultRowHeight+2*StringGrid1.GridLineWidth;
  ComboBox1.Height:=Edit1.Height; // doesn't work
  SpinEdit1.Height:=Edit1.Height; // doesn't work

  oSelected:=nil; oTopItem:=nil;
//  try IsBusy:=True; TreeView1.FullExpand; finally IsBusy:=False; end;

  TreeToGrid;
  TreeView1.Selected:=TreeView1.Items[0];
  TreeView1.TopItem :=TreeView1.Items[0];
  FormResize(Self);

  j:=0;
  for i:=0 to Pred(TreeView1.Items.Count) do with TreeView1 do begin
      Rect:=Items[i].DisplayRect(True);
      j:=Max(j,Rect.Right);
      end;
  StringGrid1.Width:=Max(150,TreeView1.ClientWidth-j-16);

  OpenDialog1.InitialDir:=SettingsPath;
  OpenDialog2.InitialDir:=OpenDialog1.InitialDir;
  SettingsFilterIndex:=0; SettingsDirectory:=OpenDialog1.InitialDir;

  for i:=Ord(High(TSettings)) downto Ord(Low(TSettings)) do begin
      DefaultSettingsEnabled[TSettings(i)]:=True;
      DefaultSettingsString [TSettings(i)]:='';
      if TreeView1.Items[i].Text=ImageSectionText then
         for j:=i+1 to i+4 do DefaultSettingsEnabled[TSettings(j)]:=False;
      end;

//TreeView1.Items[0].Collapse(True); // doesn't work, reason unknown; do it manually:
  with TreeView1 do
    for i:=Pred(Items.Count) downto 0 do with Items[i] do
        if Expanded then Collapse(False);

  MenuItemDefaultSkinSeamlessWalls.Checked              :=True;
  MenuItemDefaultSkinTiledWalls.Checked                 :=not MenuItemDefaultSkinSeamlessWalls.Checked;
  MenuItemDefaultSkinBrickWalls.Checked                 :=False;
  MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked:=True;
end;

procedure TOptionsForm.FormDestroy(Sender: TObject);
begin
  ScratchFont.Free; ScratchFont:=nil; OptionsForm:=nil;
end;

procedure TOptionsForm.ApplicationOnActivate(Sender: TObject);
var OldVolume:Integer; //s:String;
begin
  if MainForm.Music<>nil then begin
     OldVolume:=MainForm.Music.Volume64Kibi;
     MainForm.Music.OnActivateApplication;
     if OldVolume<>MainForm.Music.Volume64Kibi then begin
        {
        // update the sound volume with the value set from the outside;
        // (this is not fully implemented; the import is correct, but
        // exiting the window by pressing 'Cancel' doesn't restore the
        // original value)
        s:=IntToStr(((MainForm.Music.Volume64Kibi*100)+32767) div 65535);
        Modified:=Modified or (s<>SettingsString[stSoundVolume]);
        SettingsString[stSoundVolume]:=s;
        if Integer(TreeView1.Items[Ord(stSoundVolume)].Data)<>0 then
           StringGrid1.Cells[0,Integer(TreeView1.Items[Ord(stSoundVolume)].Data)]:=SettingsString[stSoundVolume];
        if SpinEdit1.Visible and (RowToSettings(EditorRow)=stSoundVolume) then
           SpinEdit1.Text:=SettingsString[stSoundVolume];
        ShowStatus;
        }
        end;
     end;
end;

procedure TOptionsForm.FormActivate(Sender: TObject);
var i,j:Integer; oSelected:TTreeNode;
begin
  MainForm.FormDeactivate(Sender);

  Application.OnActivate:=ApplicationOnActivate;
  Application.OnHint:=ShowHint;
  StatusBar1.Panels[1].Text:='';
  Modified:=False; ShowStatus;
  BoardGraphicsModified:=False;
//Modified:=True; ShowStatus;

  TopPanel.Font.Color:=ApplicationHiglightedTextColor;
  if ActiveControl<>StringGrid1 then TreeView1.SetFocus;

  j:=0;
  for i:=Ord(Low(TSettings)) to Ord(High(TSettings)) do begin
      DefaultSettingsEnabled[TSettings(i)]:=DefaultSettingsEnabled[TSettings(i)]
                                            and
                                            (DefaultSettingsString [TSettings(i)]<>'');
      if DefaultSettingsEnabled[TSettings(i)] then Inc(j);
      end;
  DefaultBtn.Visible:=j<>0;

  if TreeView1.Selected<>nil then
     DefaultBtn.Enabled:=DefaultSettingsEnabled[TSettings(TreeView1.Selected.AbsoluteIndex)];

  ApplyBtn.Tag:=0; // 0: the data hasn't been updated by means of the 'Apply' button

  MenuItemOpenFile.Enabled:=Assigned(OpenForm) and (not OpenForm.Visible);
  MenuItemLoadSkin.Enabled:=MenuItemOpenFile.Enabled;
  MenuItemDefaultSkin.Enabled:=MenuItemOpenFile.Enabled;
  MenuItemRecentSkins.Enabled:=MenuItemOpenFile.Enabled and (MenuItemRecentSkins.Count>2); // '-' and 'Clear recent skins' are static members of the sub-menu

  if Initialized then with TreeView1 do begin //
     Initialized:=False;

     if Assigned(TreeView1.Selected) then begin
        // Delphi 6 compatibility:
        // force a display update; otherwise the first item in the tree view
        // doesn't show up with the wanted color and font
        oSelected:=Selected;
        Selected:=Items[Ord(stSound)];
        Selected:=oSelected;
        end;
     end;

  if      HasDefaultSkinWall then begin
          MenuItemDefaultSkinSeamlessWalls.Checked:=not StrEqual(SettingsString[stGraphicsBoardFiguresWallType],WallTypeText[wtTiledWall]);
          MenuItemDefaultSkinTiledWalls   .Checked:=not MenuItemDefaultSkinSeamlessWalls.Checked;
          MenuItemDefaultSkinBrickWalls   .Checked:=False;
          end
  else if HasDefaultSkinBrickWall then begin
          MenuItemDefaultSkinSeamlessWalls.Checked:=False;
          MenuItemDefaultSkinTiledWalls   .Checked:=False;
          MenuItemDefaultSkinBrickWalls   .Checked:=True;
          end;
end;

procedure TOptionsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if   Sender=Self then
       Action:=caHide
  else Action:=caFree;

  if   ModalResult=mrOk then
       if   Modified then
            SaveData(True)
       else MainForm.Game.SessionSmoothMoveAnimationEnabled:=MainForm.Game.SmoothMoveAnimationEnabled;
end;

procedure TOptionsForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  if ActiveControl=StatGrid then with StatGrid do
//     StatGridSelectCell(StatGrid,Col,Row,CanClose);
  CanClose:=CloseEditors(False);
  if CanClose then
     if Modified and (ActiveControl<>OKBtn) then
        case Msg(SettingsChangedText,
                 Caption,
                 MB_ICONQUESTION+MB_YESNOCANCEL) of
          IDYES   : begin ActiveControl:=OKBtn; ModalResult:=mrOk; end;
          IDNO    : Modified:=False;
          IDCANCEL: CanClose:=False;
        end; // case
end;

procedure TOptionsForm.FormResize(Sender: TObject);
var i,j:Integer; Edit:Boolean; Rect:TRect;
begin
  if OptionsForm<>nil then begin
     i:=DefaultBtn.Width+4*OKBtn.Width+6*DefaultBtn.Left;
     j:=TreeView1.Top+4*StringGrid1.DefaultRowHeight+(TreeView1.Top-(TopPanel.Top+TopPanel.Height))+BottomPanel.Height+StatusBar1.Height;
     if      ClientWidth <i then ClientWidth :=i
     else if ClientHeight<j then ClientHeight:=j
          else begin
             Edit:=HideEditors;

             HelpBtn  .Left:=ClientWidth   -HelpBtn  .Width-DefaultBtn.Left;
             CancelBtn.Left:=HelpBtn  .Left-CancelBtn.Width-DefaultBtn.Left;
             OKBtn    .Left:=CancelBtn.Left-OKBtn    .Width-DefaultBtn.Left;
             ApplyBtn .Left:=2*DefaultBtn.Left+DefaultBtn.Width;

             with TreeView1 do begin
               Height:=Self.ClientHeight-StatusBar1.Height-BottomPanel.Height-Top-(Top-TopPanel.Height);
               Width :=Self.ClientWidth-2*Left;
               Rect:=TopItem.DisplayRect(False);
               TreeView1LineCount:=Height div Max(1,(Rect.Bottom-Rect.Top));
               end;
             with StringGrid1 do begin
               Top:=TreeView1.Top+2;
               Height:=TreeView1.Height-4;//-TreeView1LineCount*GridLineWidth;
//             Width:=Min(Width,TreeView1.Width-2-GetSystemMetrics(SM_CXVSCROLL)-2*Panel1.Width);
               Width:=Min(Width,TreeView1.Width-2-GetSystemMetrics(SM_CXVSCROLL)-2*SelectBtn.Width);
               Left:=TreeView1.Left+TreeView1.Width-Width{-TreeView1.Left}-2-GetSystemMetrics(SM_CXVSCROLL);
               DefaultColWidth:=ClientWidth-GridLineWidth;
               end;

             // Delphi 6 compatibility:
             // for some reason, after 'maximize window' and 'restore window',
             // the bottom panel and the status bar don't show up on the
             // screen; the following work around fixes the problem
             with BottomPanel do begin
               if Align<>alNone then Align:=alNone;
               Top:=Self.ClientHeight-StatusBar1.Height-Height;
               Width:=Self.ClientWidth;
               end;

             with StatusBar1 do begin
               if Align<>alNone then Align:=alNone;
               Top:=Self.ClientHeight-Height;
               Width:=Self.ClientWidth;
               end;

             if Edit then ShowEditor(StringGrid1.Row);
             if Left>Screen.DeskTopLeft+Screen.DeskTopWidth -30 then Left:=Screen.DeskTopLeft+Screen.DeskTopWidth -30;
             if Top >Screen.DeskTopTop +Screen.DeskTopHeight-30 then Top :=Screen.DeskTopTop +Screen.DeskTopHeight-30;
             end;
     end;
end;

procedure TOptionsForm.ShowHint(Sender: TObject);
begin
  StatusBar1.Panels[1].Text := GetLongHint(Application.Hint);
end;

procedure TOptionsForm.ApplyBtnClick(Sender: TObject);
var oCursor:TCursor;
begin
  if (not IsBusy) and Modified and ApplyBtn.Enabled and CloseEditors(False) then begin
     oCursor:=Screen.Cursor;
     try     IsBusy:=True;
             Screen.Cursor:=crHourGlass;
             ApplyBtn.Tag:=1; // 1: the data has been updated using the 'Apply' button
             SaveData(True);
             LoadData;
     finally Screen.Cursor:=oCursor;
             IsBusy:=False;
             ShowStatus;
     end;
     end;
end;

procedure TOptionsForm.OKBtnClick(Sender: TObject);
begin
 if   Modified or (ApplyBtn.Tag<>0) then begin
      ActiveControl:=OKBtn; ModalResult:=mrOk; Close; ModalResult:=mrOk;
      end
 else CancelBtnClick(Sender);
end;

procedure TOptionsForm.CancelBtnClick(Sender: TObject);
var CloseAction:TCloseAction;
begin
  ActiveControl:=CancelBtn; Modified:=False; ShowStatus;
  if   ApplyBtn.Tag=0 then ModalResult:=mrCancel
  else ModalResult:=mrOk;
  if   Screen.ActiveForm=Self then Close
  else FormClose(Self,CloseAction);
  if   ApplyBtn.Tag=0 then ModalResult:=mrCancel
  else ModalResult:=mrOk;
end;

procedure TOptionsForm.HelpBtnClick(Sender: TObject);
begin
  MainForm.BtnHelpClick(Self);
end;

procedure TOptionsForm.DefaultBtnClick(Sender: TObject);
var oCursor:TCursor;
begin
  if DefaultBtn.Enabled and
     (TreeView1.Selected<>nil) then with TreeView1 do begin
     oCursor:=Screen.Cursor;
     try     Screen.Cursor:=crHourGlass;
             SetToDefaultValue(Selected);
     finally Screen.Cursor:=oCursor;
     end;
     end;
end;

procedure TOptionsForm.ShowStatus;
begin
  StatusBar1.Panels[0].Text:=OKChangedText[Modified];
  if ApplyBtn.Enabled<>Modified then ApplyBtn.Enabled:=Modified;
  MenuItemExportSkin.Enabled:=Assigned(MainForm.GamePictures) and MainForm.GamePictures.Initialized;
  MenuItemPasteFromClipboardFull.Enabled:=Clipboard.HasFormat(CF_TEXT);
  MenuItemPasteFromClipboardSupplementary.Enabled:=MenuItemPasteFromClipboardFull.Enabled;
end;

procedure TOptionsForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if   Key=VK_F1 then
       HelpBtnClick(Sender)
  else if   (Key=VK_ESCAPE) or (Key=VK_BROWSER_BACK) then
            if   SelectBtn.Visible {Panel1.Visible} then begin
                 SelectBtn.Hide; //Panel1.Hide;
                 HideEditors; ActiveControl:=StringGrid1; Update;
                 end
            else if   EscapeEnabled then
                      if   Modified or
                           (ActiveControl=Edit1) or
                           (ActiveControl=ComboBox1) or
                           (ActiveControl=SpinEdit1) then //
                      else CancelBtnClick(Sender)
                 else EscapeEnabled:=True
  else if Key=VK_F12 then
          if WindowState=wsMaximized then WindowState:=wsNormal
          else WindowState:=wsMaximized
  else if Sender=TreeView1 then
          if   Key=VK_RIGHT then begin
               StringGrid1.SetFocus;
               Key:=0;
               end
          else
  else;
end;

procedure TOptionsForm.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button=mbRight) and (not Modified) then
     CancelBtnClick(Sender);
end;

procedure TOptionsForm.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if not IsBusy then with TreeView1 do begin
    TopPanel.Caption:=SPACE+FullItemName(Selected);
    if (oSelected<>nil) and (oSelected.Data<>nil) then;
    if (Selected <>nil) and (Selected .Data<>nil) then;
    oSelected:=Selected;
    if StringGrid1.Row<>Integer(Selected.Data) then
       StringGrid1.Row:=Integer(Selected.Data);
    //with StringGrid1 do Selection:=TGridRect(Rect(0,Row,0,Row));
    StatusBar1.Panels[1].Text:=SettingsHint  [TSettings(Selected.AbsoluteIndex)];
    DefaultBtn.Enabled       :=DefaultSettingsEnabled[TSettings(Selected.AbsoluteIndex)];
    end;
end;

procedure TOptionsForm.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if (TreeView1.TopItem<>oTopItem) and Assigned(StringGrid1) then with TreeView1 do begin
     oTopItem:=TopItem; StringGrid1.TopRow:=Integer(TopItem.Data);
     end;
  DefaultDraw:=True;
  if      (cdsFocused in State) or (cdsSelected in State) then
          TreeView1.Canvas.Font.Color:=clHighlightText
  else if Node.HasChildren then
          TreeView1.Canvas.Font.Color:=FontColorBranchNode
  else    TreeView1.Canvas.Font.Color:=FontColorLeafNode;
end;

function TOptionsForm.FullItemName(Node:TTreeNode):String;
begin
  Result:='';
  while Node<>nil do begin
    if   Result='' then
         Result:=Node.Text
    else Result:=Node.Text+SUB_TITLE_SEPARATOR+Result;
    Node:=Node.Parent;
    end;
end;

procedure TOptionsForm.TreeToGrid;
var i,LineCount:Integer;
begin
  if not IsBusy then with TreeView1 do begin
     HideEditors;
     oTopItem:=TopItem; LineCount:=0;
     for i:=0 to Pred(Items.Count) do
         if Items[i].IsVisible then Inc(LineCount);

     if StringGrid1.RowCount<>LineCount then
        try    StringGrid1.RowCount:=LineCount; // so 'PgDn' works correctly
        except on E:Exception do Error(E.Message,Application.Title);
        end;

     LineCount:=0;
     for i:=0 to Pred(Items.Count) do with StringGrid1 do
         if (Items[i].IsVisible) and (LineCount<StringGrid1.RowCount) then begin
            if    SettingsEditor[TSettings(i)]=seNone then
                  Cells[0,LineCount]:=''
             else Cells[0,LineCount]:=SettingsString[TSettings(i)];
             Objects[0,LineCount]:=Pointer(i);
             Items[i].Data:=Pointer(LineCount);
             Inc(LineCount);
            end
         else Items[i].Data:=Pointer(0);

     StringGrid1.TopRow:=Integer(TopItem.Data);
     if Selected<>nil then StringGrid1.Row:=Integer(Selected.Data);
     end;
end;

procedure TOptionsForm.TreeView1Collapsed(Sender: TObject; Node: TTreeNode);
begin
  TreeView1.Selected:=Node;
  TreeToGrid;
end;

procedure TOptionsForm.TreeView1Expanded(Sender: TObject; Node: TTreeNode);
begin
  if not Initialized then begin
     TreeView1.Selected:=Node;
     TreeToGrid;
     end;
end;

procedure TOptionsForm.TreeView1Click(Sender: TObject);
begin
  //StatusBar1.Panels[1].Text:='Click';
end;

procedure TOptionsForm.TreeView1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var CanSelect:Boolean;
begin
  if   Key=VK_RETURN then with TreeView1 do begin
       if Selected<>nil then
          if   Selected.HasChildren then begin
               Selected.Expanded:=not Selected.Expanded;
               TreeToGrid;
               end
          else if   SettingsEditor[TSettings(Selected.AbsoluteIndex)]<>seNone then begin
                    StringGrid1SelectCell(Sender,0,Integer(Selected.Data),CanSelect);
                    if CanSelect then with StringGrid1 do
                       begin Col:=0; Row:=Integer(Selected.Data);
                             ShowEditor(Integer(Selected.Data));
                       end;
                    end;
       end
  else if Key=VK_F12 then
          if WindowState=wsMaximized then WindowState:=wsNormal
          else WindowState:=wsMaximized
  else if (Key=VK_RIGHT) or (Key=VK_LEFT) then
          StringGrid1.SetFocus
  else;
end;

procedure TOptionsForm.MenuItemExitClick(Sender: TObject);
begin
  if   CloseEditors(False) and Modified then
       OKBtnClick(Sender)
  else Close;
end;

procedure TOptionsForm.MenuItemTopMenuExitClick(Sender: TObject);
begin
  // issue a "Commit or drop changes?" question if the settings have been
  // modified;
  // the reason is that it's not well-defined what "exit" should do here
  // because no message appears on the status bar when the mouse hovers over
  // a top-menu item, i.e., there are no "Apply any changes and return to the
  // previous window" help-text visible on the screen;
  // so this is the same dilemma as when the user clicks the "X" button on the
  // window frame, where the program also issues the "Commit changes?" question;
  Close;
end;

procedure TOptionsForm.MenuItemSaveTreeClick(Sender: TObject);
begin
  try    TreeView1.SaveToFile('Tree.txt');
  except on E:Exception do Error(TEXT_TASK_FAILED+NL+NL+TEXT_FAILURE_DESCRIPTION+NL+E.Message,Application.Title);
  end;
end;

function  TOptionsForm.GetSettingsString(No:TSettings):String;
begin
  Result:=FSettingsString[No];
end;

procedure TOptionsForm.SetSettingsString(No:TSettings; const s:String);
begin
  if SettingsString[No]<>s then Modified:=True;
  FSettingsString[No]:=s;
end;

procedure TOptionsForm.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  ARect: TRect; AState: TGridDrawState);
var i:Integer; s:String;
begin
  with StringGrid1 do
    begin  if   ((gdFocused in AState) {or (gdSelected in AState)}) then
                //and
                //(SettingsEditor[TSettings(ARow)]<>seNone) then
                if   (SettingsEditor[RowToSettings(ARow)]=seColor) and
                     SafeStrToInt(RGB_BGR(Copy(Cells[ACol,ARow],1,6)),True,i) then
                     begin Canvas.Font .Color:=ContrastColor(TColor(i));
                           Canvas.Brush.Color:=TColor(i);
                     end
                else begin Canvas.Font .Color:=clHighlightText;
                           Canvas.Brush.Color:=clHighlight;
                     end
           else begin      Canvas.Font .Color:=TreeView1.Font.Color;
                           Canvas.Brush.Color:=TreeView1.Color;
                end;
           Canvas.FillRect(ARect);
           s:=Cells[ACol,ARow];
           if s<>'' then
              begin
                Dec(ARect.Right); Dec(ARect.Bottom);
                ExtTextOut(Canvas.Handle, ARect.Left + 2, ARect.Top + 2, ETO_CLIPPED or
                  ETO_OPAQUE, @ARect, PChar(S), Length(S), nil);
              end;
           if (gdSelected in AState)  //and
              //(gdFocused  in AState)  //and
              //StringGrid1.Focused //and
              //(SettingEditor[Integer(Objects[0,ARow])]<>peNone) and
              then
              begin Inc(ARect.Left); Inc(ARect.Top);
                    Canvas.DrawFocusRect(ARect);
              end;
     end;
end;
{
procedure TOptionsForm.LoadData;
var i:TSettings; FileName:String; SL:TStringList;
begin
  for i:=Low(i) to High(i) do with TreeView1 do
      if   SettingsEditor[i]=seNone then
           FSettingsString[i]:=''
      else FSettingsString[i]:=Trim(TreeView1.Items[Ord(i)].Text);

  FileName:=MainForm.ApplicationDataPath+'t1.set';
  if FileExists(FileName) then
     try    SL:=TStringList.Create;
            try     SL.LoadFromFile(FileName);
                    for i:=Low(i) to High(i) do
                        SettingsString[i]:=SL.Strings[Ord(i)];
            finally SL.Free;
            end;
     except
       on E:Exception do begin
            Error(Format(OpenFileFailedShortText__+NL+NL+
                          ReasonText+NL+E.Message,[FileName]),
                          OpenDialog1.Title);
          end;
     end;
end;
}

procedure TOptionsForm.LoadData;
var i,j:Integer; p:TPictureType;

  procedure LoadFont(const Font:TFont; Index:TSettings);
  begin
    with Font do begin
      SettingsString[Index                              ]:=Name;
      SettingsString[Succ(Index)                        ]:=IntToStr(Size);
      SettingsString[Succ(Succ(Index))                  ]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
      SettingsString[Succ(Succ(Succ(Index)))            ]:=BooleanText[fsBold in Style];
      SettingsString[Succ(Succ(Succ(Succ(Index))))      ]:=BooleanText[fsItalic in Style];
      SettingsString[Succ(Succ(Succ(Succ(Succ(Index)))))]:=BooleanText[fsUnderline in Style];
      end;
  end;

begin // LoadData
  for i:=Ord(Low(TSettings)) to Ord(High(TSettings)) do SettingsString[TSettings(i)]:='';

  SettingsString[stMoveAnimationEnabled]:=BooleanText[MainForm.Game.MoveAnimationEnabled];
  SettingsString[stSmoothMoveAnimationEnabled]:=BooleanText[MainForm.Game.SmoothMoveAnimationEnabled];
  SettingsString[stPlayerDirectionAnimationEnabled]:=BooleanText[MainForm.Game.PlayerDirectionAnimationEnabled];
//SettingsString[stSmoothMoveAnimationThresholdEnabled]:=BooleanText[MainForm.Game.SmoothMoveAnimationThresholdEnabled];
//SettingsString[stSmoothMoveAnimationThresholdMaxPixelsPerMove]:=IntToStr(MainForm.Game.SmoothMoveAnimationThresholdMaxPixelsPerMove);
  SettingsString[stMoveAnimationDoMove]:=IntToStr(MainForm.Game.AnimateDoMoveMS);
  SettingsString[stMoveAnimationUndoMove]:=IntToStr(MainForm.Game.AnimateUndoMoveMS);
  SettingsString[stMoveAnimationReplayMoves]:=IntToStr(MainForm.Game.AnimateReplayMovesMS);
  SettingsString[stMoveAnimationEnabledOnMouseWheelUpDown]:=BooleanText[MainForm.Game.AnimateMovesOnMouseWheelUpDown];
  SettingsString[stMoveAnimationForkliftDrivingEnabled]:=BooleanText[MainForm.Game.ForkliftDrivingEnabled];
  SettingsString[stMoveAnimationForkliftDrivingDriveInReverseSquares]:=IntToStr(MainForm.Game.ForkliftDrivingDriveInReverseSquares);

  SettingsString[stPathFindingMaxTime]:=IntToStr(MainForm.Game.PathFindingMaxTimeMS);
  SettingsString[stPathFindingPushesLowerBoundCalculationMaxTime]:=IntToStr(MainForm.Deadlocks.CalculatePushesLowerBoundTimeLimitMS);
  SettingsString[stPathFindingOptimizeMovesOrPushes]:=PathFindingOptimizationTypeText[MainForm.Game.PathFindingOptimizeMoves];
  SettingsString[stPathFindingShowLegalMovesEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.Enabled];
  SettingsString[stPathFindingShowLegalMovesSize]:=IntToStr(MainForm.GameViewer.LegalMovesInfo.Size);
  SettingsString[stPathFindingShowLegalMovesTransparentImage]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.TransparentImage];
  SettingsString[stPathFindingShowLegalMovesTransparency]:=IntToStr(MainForm.GameViewer.LegalMovesInfo.Transparency);
  SettingsString[stPathFindingShowLegalMovesPlayerCursorEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.PlayerCursor.Enabled];
  SettingsString[stPathFindingShowLegalMovesPlayerCursorColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.PlayerCursor.Color),6));
  SettingsString[stPathFindingShowLegalMovesPlayerCursorPenWidth]:=IntToStr(MainForm.GameViewer.LegalMovesInfo.PlayerCursor.PenWidth);
  SettingsString[stPathFindingShowLegalMovesPlayerCursorSize]:=IntToStr(MainForm.GameViewer.LegalMovesInfo.PlayerCursor.Size);
  SettingsString[stPathFindingShowLegalMovesPlayerCursorshadowEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.PlayerCursor.ShadowEnabled];
  SettingsString[stPathFindingShowLegalMovesPlayerCursorShadowColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.PlayerCursor.ShadowColor),6));
  SettingsString[stPathFindingShowLegalMovesBoxCursorEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.BoxCursor.Enabled];
  SettingsString[stPathFindingShowLegalMovesBoxCursorColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.BoxCursor.Color),6));
  SettingsString[stPathFindingShowLegalMovesBoxCursorPenWidth]:=IntToStr(MainForm.GameViewer.LegalMovesInfo.BoxCursor.PenWidth);
  SettingsString[stPathFindingShowLegalMovesBoxCursorSize]:=IntToStr(MainForm.GameViewer.LegalMovesInfo.BoxCursor.Size);
  SettingsString[stPathFindingShowLegalMovesBoxCursorshadowEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.BoxCursor.ShadowEnabled];
  SettingsString[stPathFindingShowLegalMovesBoxCursorShadowColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.BoxCursor.ShadowColor),6));
  SettingsString[stPathFindingShowLegalMovesDeadlocksEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.DeadlocksEnabled];
  SettingsString[stPathFindingShowLegalMovesDeadlocksColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.DeadlocksColor),6));
  SettingsString[stPathFindingShowLegalMovesJumpMovesEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.JumpMovesEnabled];
  SettingsString[stPathFindingShowLegalMovesJumpMovesColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.JumpMovesBackgroundColor),6));
  SettingsString[stPathFindingShowLegalMovesJumpMovesColorsText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.JumpMovesTextColor),6));
  SettingsString[stPathFindingShowLegalMovesJumpMovesColorsTextShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GameViewer.LegalMovesInfo.JumpMovesTextShadowColor),6));
  SettingsString[stPathFindingShowLegalMovesAnimationPlayerAnimationEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.PlayerAnimationEnabled];
  SettingsString[stPathFindingShowLegalMovesAnimationBoxAnimationEnabled]:=BooleanText[MainForm.GameViewer.LegalMovesInfo.BoxAnimationEnabled];
  SettingsString[stPathFindingShowBoardCoordinates]:=BooleanText[MainForm.ShowBoardCoordinates];

  SettingsString[stSolutionsPreferredType]:=SolutionTypeText[MainForm.ShowSolutionMoves];
  SettingsString[stSolutionsSaveAutomatically]:=BooleanText[MainForm.Game.SaveBestSolutionsAutomatically];
  SettingsString[stSolutionsSaveOldSolutionsAfterFindingBetterOnes]:=BooleanText[MainForm.Game.SaveOldSolutionsAfterFindingBetterOnes];
  SettingsString[stSolutionsSaveOldSolutionsAfterClipboardImport]:=BooleanText[MainForm.Game.SaveOldSolutionsAfterClipboardImport];
  SettingsString[stSolutionsRequireAtLeastOnePush]:=BooleanText[MainForm.Game.SolutionsRequireAtLeastOnePush];
  SettingsString[stSolutionsAnimationPlayerAnimationEnabled]:=BooleanText[MainForm.GameViewer.SolutionsInfo.PlayerAnimationEnabled];
  SettingsString[stSolutionsAnimationBoxAnimationEnabled]:=BooleanText[MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled];
//SettingsString[stSolutionsScoreMetricsWeightsBoxLines]:=IntToStr(SokGame_.ScoreMetricsWeights.BoxLines);
//SettingsString[stSolutionsScoreMetricsWeightsBoxChanges]:=IntToStr(SokGame_.ScoreMetricsWeights.BoxChanges);
//SettingsString[stSolutionsScoreMetricsWeightsPushingSessions]:=IntToStr(SokGame_.ScoreMetricsWeights.PushingSessions);
  SettingsString[stSolutionsSecondaryMetricsInSolutionButtonHint]:=BooleanText[MainForm.SecondaryMetricsInSolutionButtonHint];
  SettingsString[stSolutionsAllowDeletionOfBestSolutionsInSnapshotsWindow]:=BooleanText[SnapshotsForm.AllowDeletionOfBestSolutions];
  SettingsString[stSolutionsErrorRecoveryEnabled]:=BooleanText[MainForm.ErrorRecoveryEnabled];

  SettingsString[stSnapshotsSaveAutomatically]:=BooleanText[MainForm.Game.SaveSnapshotsAutomatically];
  SettingsString[stSnapshotsSecondaryMetricsInTitles]:=BooleanText[MainForm.Game.SecondaryMetricsInTitles];
  SettingsString[stReverseModeShowBoxStartPositionsAsGoals]:=BooleanText[MainForm.Game.ShowBoxStartPositionsAsGoalsInReverseMode];
  SettingsString[stReverseModeJumpsAllowedAfterFirstBoxMove]:=BooleanText[MainForm.Game.JumpsAllowedAfterFirstBoxMoveInReverseMode];

  SettingsString[stDeadlockDetectionEnabled]:=BooleanText[MainForm.Game.DeadlockDetection.Enabled];
  SettingsString[stDeadlockDetectionType]:=DeadlockDetectionTypeText[MainForm.Game.DeadlockDetection.DeadlockDetectionType];
  SettingsString[stDeadlockDetectionBlockMoves]:=BooleanText[MainForm.Game.DeadlockDetection.BlockMoves];
  SettingsString[stDeadlockDetectionLogEnabled]:=BooleanText[MainForm.Game.DeadlockDetection.LogEnabled];
  SettingsString[stDeadlockDetectionShowSimpleDeadSquaresEnabled]:=BooleanText[MainForm.ShowSimpleDeadSquaresEnabled];
  SettingsString[stDeadlockDetectionShowSimpleDeadSquaresColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.ShowSimpleDeadSquaresColor),6));
  SettingsString[stDeadlockDetectionShowSimpleDeadSquaresTransparencyPct]:=IntToStr(MainForm.ShowSimpleDeadSquaresTransparencyPct);

  SettingsString[stUndoRedoCombinedMoves]:=BooleanText[MainForm.UndoRedoCombinedMoves];
  SettingsString[stOptimizeMovesBetweenPushes]:=BooleanText[MainForm.OptimizeMovesBetweenPushes];
  SettingsString[stOnLeftClickEmptySquareAction]:=OnLeftClickEmptySquareActionText[MainForm.OnLeftClickEmptySquareAction];
  SettingsString[stOnRightClickAction]:=OnRightClickActionText[MainForm.OnRightClickAction];
  SettingsString[stOnShiftKeyAction]:=OnShiftKeyActionText[MainForm.OnShiftKeyAction];

  SettingsString[stTimingEnabled]:=BooleanText[MainForm.Game.TimingEnabled];
  SettingsString[stTimingIdleTimeThresholdEnabled]:=BooleanText[MainForm.Game.TimingIdleTimeThresholdEnabled];
  SettingsString[stTimingIdleTimeThresholdSeconds]:=IntToStr(MainForm.Game.TimingIdleTimeThresholdMS div 1000);

  if MainForm.WindowState=wsNormal then begin
     SettingsString[stWindowsMainLeft]                 :=IntToStr(MainForm.Left);
     SettingsString[stWindowsMainTop]                  :=IntToStr(MainForm.Top);
     SettingsString[stWindowsMainWidth]                :=IntToStr(MainForm.Width);
     SettingsString[stWindowsMainHeight]               :=IntToStr(MainForm.Height);
     end
  else begin
     SettingsString[stWindowsMainLeft]                 :=DefaultSettingsString[stWindowsMainLeft];
     SettingsString[stWindowsMainTop]                  :=DefaultSettingsString[stWindowsMainTop];
     SettingsString[stWindowsMainWidth]                :=DefaultSettingsString[stWindowsMainWidth];
     SettingsString[stWindowsMainHeight]               :=DefaultSettingsString[stWindowsMainHeight];
     end;
  SettingsString[stWindowsMainBoardDimensionsInTitleLine]:=BooleanText[MainForm.BoardDimensionsInTitleLine];
  SettingsString[stWindowsMainCollectionNameInTitleLine]:=BooleanText[MainForm.CollectionNameInTitleLine];

  if OpenForm.WindowState=wsNormal then begin
     SettingsString[stWindowsOpenLeft]                 :=IntToStr(OpenForm.Left);
     SettingsString[stWindowsOpenTop]                  :=IntToStr(OpenForm.Top);
     SettingsString[stWindowsOpenWidth]                :=IntToStr(OpenForm.Width);
     SettingsString[stWindowsOpenHeight]               :=IntToStr(OpenForm.Height);
     end
  else begin
     SettingsString[stWindowsOpenLeft]                 :=DefaultSettingsString[stWindowsOpenLeft];
     SettingsString[stWindowsOpenTop]                  :=DefaultSettingsString[stWindowsOpenTop];
     SettingsString[stWindowsOpenWidth]                :=DefaultSettingsString[stWindowsOpenWidth];
     SettingsString[stWindowsOpenHeight]               :=DefaultSettingsString[stWindowsOpenHeight];
     end;
  SettingsString[stWindowsOpenDiskGroupBoxWidth]       :=IntToStr(OpenForm.DiskGroupBox.Width);
  SettingsString[stWindowsOpenDiskGroupBoxHeight]      :=IntToStr(OpenForm.DiskGroupBox.Height);
  SettingsString[stWindowsOpenFolderListBoxWidth]      :=IntToStr(OpenForm.DirectoryListBox1.Width);
  SettingsString[stWindowsOpenLevelPreviewHeight]      :=IntToStr(OpenForm.GameBoardPanel.Height);
  SettingsString[stWindowsOpenSolutionColumns]:=IntToStr(OpenForm.SolutionColumns);
  SettingsString[stWindowsOpenIncludeBuiltinSolutions]:=BooleanText[OpenForm.MenuItemIncludeBuiltinSolutions.Checked];

  SettingsString[stWindowsOpenToolTipsEnabled]         :=BooleanText[OpenForm.ToolTips.Enabled];
  SettingsString[stWindowsOpenToolTipsOffsetX]         :=IntToStr(OpenForm.ToolTips.OffsetX);
  SettingsString[stWindowsOpenToolTipsOffsetY]         :=IntToStr(OpenForm.ToolTips.OffsetY);

  if ToolsForm.WindowState=wsNormal then begin
     SettingsString[stWindowsToolsLeft]                :=IntToStr(ToolsForm.Left);
     SettingsString[stWindowsToolsTop]                 :=IntToStr(ToolsForm.Top);
     SettingsString[stWindowsToolsWidth]               :=IntToStr(ToolsForm.Width);
     SettingsString[stWindowsToolsHeight]              :=IntToStr(ToolsForm.Height);
     end
  else begin
     SettingsString[stWindowsToolsLeft]                :=DefaultSettingsString[stWindowsToolsLeft];
     SettingsString[stWindowsToolsTop]                 :=DefaultSettingsString[stWindowsToolsTop];
     SettingsString[stWindowsToolsWidth]               :=DefaultSettingsString[stWindowsToolsWidth];
     SettingsString[stWindowsToolsHeight]              :=DefaultSettingsString[stWindowsToolsHeight];
     end;
  SettingsString[stWindowsToolsCollectionNameInTitleLine]:=BooleanText[ToolsForm.CollectionNameInTitleLine];

  if SnapshotsForm.WindowState=wsNormal then begin
     SettingsString[stWindowsSnapshotsLeft]           :=IntToStr(SnapshotsForm.Left);
     SettingsString[stWindowsSnapshotsTop]            :=IntToStr(SnapshotsForm.Top);
     SettingsString[stWindowsSnapshotsWidth]          :=IntToStr(SnapshotsForm.Width);
     SettingsString[stWindowsSnapshotsHeight]         :=IntToStr(SnapshotsForm.Height);
     end
  else begin
     SettingsString[stWindowsSnapshotsLeft]           :=DefaultSettingsString[stWindowsSnapshotsLeft];
     SettingsString[stWindowsSnapshotsTop]            :=DefaultSettingsString[stWindowsSnapshotsTop];
     SettingsString[stWindowsSnapshotsWidth]          :=DefaultSettingsString[stWindowsSnapshotsWidth];
     SettingsString[stWindowsSnapshotsHeight]         :=DefaultSettingsString[stWindowsSnapshotsHeight];
     end;

  if LevelSetForm.WindowState=wsNormal then begin
     SettingsString[stWindowsInternalClipboardLeft]    :=IntToStr(LevelSetForm.Left);
     SettingsString[stWindowsInternalClipboardTop]     :=IntToStr(LevelSetForm.Top);
     SettingsString[stWindowsInternalClipboardWidth]   :=IntToStr(LevelSetForm.Width);
     SettingsString[stWindowsInternalClipboardHeight]  :=IntToStr(LevelSetForm.Height);
     end
  else begin
     SettingsString[stWindowsInternalClipboardLeft]    :=DefaultSettingsString[stWindowsInternalClipboardLeft];
     SettingsString[stWindowsInternalClipboardTop]     :=DefaultSettingsString[stWindowsInternalClipboardTop];
     SettingsString[stWindowsInternalClipboardWidth]   :=DefaultSettingsString[stWindowsInternalClipboardWidth];
     SettingsString[stWindowsInternalClipboardHeight]  :=DefaultSettingsString[stWindowsInternalClipboardHeight];
     end;

  SettingsString[stSelectionCursorType]:=CursorTypeText[MainForm.SelectionCursorType];
  SettingsString[stDragCursorType]:=CursorTypeText[MainForm.DragCursorType];
  SettingsString[stTrackingCursorType]:=CursorTypeText[MainForm.TrackingCursorType];

  SettingsString[stToolsSolverFileName]:=MainForm.Solver.PluginFileName;
  if StrEqual(SettingsString[stToolsSolverFileName],MainForm.Solver.DefaultPluginFileName) then
     SettingsString[stToolsSolverFileName]:=DEFAULT_VALUE;
  SettingsString[stToolsSolverPriority]:=ThreadPriorityText[MainForm.Solver.Priority];
  SettingsString[stToolsSolverAddPluginNameToSolutionTitles]:=BooleanText[MainForm.Solver.AddPluginNameToSolutionTitles];
  SettingsString[stToolsSolverAddDateToSolutionNotes]:=BooleanText[MainForm.Solver.AddDateToSolutionNotes];
  SettingsString[stToolsSolverStatisticsEnabled]:=BooleanText[MainForm.Solver.StatisticsEnabled];
  SettingsString[stToolsSolverTimeLimitEnabled]:=BooleanText[MainForm.Solver.TimeLimitEnabled];
  SettingsString[stToolsSolverTimeLimitSeconds]:=IntToStr(MainForm.Solver.TimeLimitMS div ONE_THOUSAND);

  SettingsString[stToolsOptimizerFileName]:=MainForm.Optimizer.PluginFileName;
  if StrEqual(SettingsString[stToolsOptimizerFileName],MainForm.Optimizer.DefaultPluginFileName) then
     SettingsString[stToolsOptimizerFileName]:=DEFAULT_VALUE;
  SettingsString[stToolsOptimizerPriority]:=ThreadPriorityText[MainForm.Optimizer.Priority];
  SettingsString[stToolsOptimizerAddPluginNameToSolutionTitles]:=BooleanText[MainForm.Optimizer.AddPluginNameToSolutionTitles];
  SettingsString[stToolsOptimizerAddDateToSolutionNotes]:=BooleanText[MainForm.Optimizer.AddDateToSolutionNotes];
  SettingsString[stToolsOptimizerStatisticsEnabled]:=BooleanText[MainForm.Optimizer.StatisticsEnabled];
  SettingsString[stToolsOptimizerDiscardDethronedSolutions]:=BooleanText[MainForm.Optimizer.DiscardDethronedSolutions];
  SettingsString[stToolsOptimizerSeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly]:=BooleanText[MainForm.Optimizer.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly];
  SettingsString[stToolsOptimizerTimeLimitEnabled]:=BooleanText[MainForm.Optimizer.TimeLimitEnabled];
  SettingsString[stToolsOptimizerTimeLimitSeconds]:=IntToStr(MainForm.Optimizer.TimeLimitMS div ONE_THOUSAND);
  SettingsString[stToolsOptimizerAlternatingOptimizationsEnabled]:=BooleanText[ToolsForm.SettingsMenuAlternatingOptimizationsEnabled.Checked];
  SettingsString[stToolsOptimizerAlternatingOptimizationsType]:=AlternatingOptimizationsText[ToolsForm.AlternatingOptimizations];
  SettingsString[stToolsOptimizerAlternatingOptimizationsTrigger]:=AlternatingOptimizationsTriggerTypeText[ToolsForm.SettingsMenuAlternatingOptimizationsTriggerOnImprovements.Checked];
  SettingsString[stToolsOptimizerAlternatingOptimizationsRepeatEnabled]:=BooleanText[ToolsForm.SettingsMenuAlternatingOptimizationsRepeat.Checked];
  SettingsString[stToolsOptimizerAlternatingOptimizationsRepeatType]:=AlternatingOptimizationsRepeatTypeText[ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines.Checked];
  SettingsString[stToolsOptimizerIntervalsWallifyBoxesOutsideRangeOfPushes]:=BooleanText[MainForm.Optimizer.WallifyBoxesOutsideRangeOfPushesToBeOptimized];
  SettingsString[stToolsOptimizerIntervalsSubIntervalOverlapPct]:=IntToStr(MainForm.Optimizer.SubIntervalOverlapPct);

  SettingsString[stToolsGeneratorPriority]:=ThreadPriorityText[MainForm.Generator.Priority];

  SettingsString[stToolsCaptureSnapToNearbyEdges]:=BooleanText[ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked];
  SettingsString[stToolsCaptureGridColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CaptureForm.Editor.GridColor),6));
  SettingsString[stToolsCaptureGridShadowColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CaptureForm.Editor.GridShadowColor),6));
  SettingsString[stToolsCaptureTextsDefaultSkinFileName]:=CaptureForm.DefaultSkinFileNameText;
  SettingsString[stToolsCaptureAppendNewLevelsToCollectionsCheckForDuplicates]:=BooleanText[CaptureForm.Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections];

  SettingsString[stToolsDuplicateFinderAdvancedIdentityCheck]:=BooleanText[DuplicatesForm.AdvancedIdentityCheck];
  SettingsString[stToolsDuplicateFinderCurrentLevelMatchThresholdPercent]:=IntToStr(DuplicatesForm.MatchThresholdPercent);

  SettingsString[stToolsSkinExportBaseFormatType]:=SkinExportFormatTypeText[SkinExportFormatType];

  SettingsString[stAddFileFormatDescriptionToFiles]:=BooleanText[MainForm.AddFileFormatDescriptionToFiles];
  SettingsString[stPrettyPrintGamesEnabled]:=BooleanText[MainForm.Game.PrettyPrintGamesEnabled];
  SettingsString[stCopyLevelToClipboardFillFloors]:=BooleanText[MainForm.CopyLevelToClipboardFillFloors];
  SettingsString[stCopyLevelToClipboardFloorFillCharacter]:=StrWithDoubleQuotes(MainForm.CopyLevelToClipboardFloorFillCharacter);
  SettingsString[stCopyLevelCollectionToClipboardIncludeLevelComments]:=BooleanText[MainForm.CopyLevelCollectionToClipboardIncludeLevelComments];
  SettingsString[stCopyLevelToClipboardPreserveCombinedMoves]:=BooleanText[MainForm.CopyLevelToClipboardPreserveCombinedMoves];
  SettingsString[stCopyLevelToClipboardBasedOnSolutionSliceIncludeTitles]:=BooleanText[MainForm.CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles];
  SettingsString[stRunLengthEncodingFloor]:=StrWithDoubleQuotes(MainForm.RunLengthEncodingFloor);

//OldHasFileAssociations:=MainForm.FileAssoc.HasFileAssociations;
//SettingsString[stAssociateFileTypes]:=BooleanText[OldHasFileAssociations];
  SettingsString[stRestoreSaveGameOnOpen]:=BooleanText[MainForm.Game.RestoreSaveGame];
  SettingsString[stAutoAdvanceWhenSolved]:=BooleanText[MainForm.AutoAdvanceWhenSolved];
  SettingsString[stMouseTrackingModeEnabled]:=BooleanText[MainForm.MouseTrackingModeEnabled];
  SettingsString[stBoardDimensionsAsText]:=Text_.BoardDimensionsAsText[MainForm.BoardDimensionsAsText];
  SettingsString[stBoardDimensionsWithFloorCount]:=BooleanText[MainForm.BoardDimensionsWithFloorCount];
  SettingsString[stOnEscapeKeyAction]:=OnEscapeKeyActionText[MainForm.OnEscapeKeyAction];
  SettingsString[stDisplayDriverChangedAction]:=DisplayDriverChangedActionText[MainForm.DisplayDriverChangedAction];
  SettingsString[stConfirmRunningMultipleInstances]:=BooleanText[MainForm.ConfirmRunningMultipleInstances];
  SettingsString[stScreenSaverEnabled]:=BooleanText[MainForm.ScreenSaverEnabled];
  SettingsString[stUseOnlyOneProcessor]:=BooleanText[MainForm.UseOnlyOneProcessor];

  SettingsString[stSoundEnabled]:=BooleanText[MainForm.Sound.Enabled];
  SettingsString[stSoundVolume]:=IntToStr(((MainForm.Music.Volume64Kibi*100)+32767) div 65535);
  SettingsString[stSoundResetSoundEffectsOnLoadingASkin]:=BooleanText[MainForm.Sound.ResetSoundEffectsOnLoadingASkin];
  for i:=Ord(Low(TSoundType)) to Ord(High(TSoundType)) do begin
      SettingsString[TSettings(3*i+Ord(stSoundMoveEnabled))]:=BooleanText[MainForm.Sound.SoundEnabled[TSoundType(i)]];
      SettingsString[TSettings(3*i+Ord(stSoundMoveFileName))]:=MainForm.Sound.SoundFileName[TSoundType(i)];
      end;
//SettingsString[stSoundFileTypeFilter]:=InternalToExternalFileTypeFilter(MainForm.Sound.SoundFileTypeFilter);

{$IFDEF MUSIC_PLAYER}
  SettingsString[stMusicEnabled]:=BooleanText[MainForm.Music.Enabled];
  SettingsString[stMusicSource]:=MusicSourceText[MainForm.Music.MusicSource];
  SettingsString[stMusicCDDrive]:=AnsiUpperCase(MainForm.Music.CDDrive+COLON);
  SettingsString[stMusicFilePath]:=MainForm.Music.MusicFilePath;
  SettingsString[stMusicPlayListFileName]:=MainForm.Music.PlayListFileName;
  SettingsString[stMusicFileTypeFilter]:=InternalToExternalFileTypeFilter(MainForm.Music.MusicFileTypeFilter);

  with MainForm.MPlayer do begin
    SettingsString[stMusicPlayerTransparency]:=BooleanText[Transparency];
    SettingsString[stMusicPlayerTransparencyPct]:=IntToStr(TransparencyPct);
    SettingsString[stMusicPlayerColor]:=MusicPlayerColorText[RGBShift];
    SettingsString[stMusicPlayerDisplayTransparencyPct]:=IntToStr(Display.TransparencyPct);
    with MPlayerDisplayPanel.Font do begin
      SettingsString[stMusicPlayerDisplayFontName]:=Name;
      SettingsString[stMusicPlayerDisplayFontSize]:=IntToStr(Size);
      SettingsString[stMusicPlayerDisplayFontColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
      SettingsString[stMusicPlayerDisplayFontBold]:=BooleanText[fsBold in Style];
      SettingsString[stMusicPlayerDisplayFontItalic]:=BooleanText[fsItalic in Style];
      SettingsString[stMusicPlayerDisplayFontUnderlined]:=BooleanText[fsUnderline in Style];
      end;
    if Display<>nil then begin
       SettingsString[stMusicPlayerDisplayShowFps]:=BooleanText[Display.ShowFps];
       SettingsString[stMusicPlayerDisplayTitleMode]:=MusicPlayerTitleModeText[Display.TitleMode];
       SettingsString[stMusicPlayerDisplayActivitiesActivity]:=ActivityText[Display.Activity];
       if Display.ActivitiesMenu<>nil then with Display do with ActivitiesMenu.MenuPanel.Font do begin
          SettingsString[stMusicPlayerDisplayActivitiesMenuTransparencyPct]:=IntToStr(ActivitiesMenu.TransparencyPct);
          SettingsString[stMusicPlayerDisplayActivitiesMenuTextShadow]:=BooleanText[ActivitiesMenu.TextShadow];
          SettingsString[stMusicPlayerDisplayActivitiesMenuColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ActivitiesMenu.MenuPanel.Color),6));
          SettingsString[stMusicPlayerDisplayActivitiesMenuColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ActivitiesMenu.ButtonFontColor[Menu_.bsEnabled]),6));
          SettingsString[stMusicPlayerDisplayActivitiesMenuColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ActivitiesMenu.ButtonFontColor[Menu_.bsFocusedEnabled]),6));
          SettingsString[stMusicPlayerDisplayActivitiesMenuColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ActivitiesMenu.ButtonFontColor[Menu_.bsDisabled]),6));
          SettingsString[stMusicPlayerDisplayActivitiesMenuColorsTextShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ActivitiesMenu.TextShadowColor[bsEnabled]),6));

          SettingsString[stMusicPlayerDisplayActivitiesMenuFontName]:=Name;
          SettingsString[stMusicPlayerDisplayActivitiesMenuFontSize]:=IntToStr(Size);
          SettingsString[stMusicPlayerDisplayActivitiesMenuFontColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
          SettingsString[stMusicPlayerDisplayActivitiesMenuFontBold]:=BooleanText[fsBold in Style];
          SettingsString[stMusicPlayerDisplayActivitiesMenuFontItalic]:=BooleanText[fsItalic in Style];
          SettingsString[stMusicPlayerDisplayActivitiesMenuFontUnderlined]:=BooleanText[fsUnderline in Style];
          end;
       end;
    end;

  with Mandala do begin
    with Engine do begin
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMin]:=IntToStr(PaletteSpinSpeed.StepsPerSecondMin);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMax]:=IntToStr(PaletteSpinSpeed.StepsPerSecondMax);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingInterval]:=IntToStr(PaletteSpinSpeed.ChangeIntervalMS);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingDeviation]:=IntToStr(PaletteSpinSpeed.ChangeDeviationPct);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMin]:=IntToStr(PaletteChangeSpeed.StepsPerSecondMin);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMax]:=IntToStr(PaletteChangeSpeed.StepsPerSecondMax);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeInterval]:=IntToStr(PaletteChangeSpeed.ChangeIntervalMS);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeDeviation]:=IntToStr(PaletteChangeSpeed.ChangeDeviationPct);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsInterval]:=IntToStr(PaletteEffectsIntervalMS);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsDeviation]:=IntToStr(PaletteEffectsDeviationPct);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternInterval]:=IntToStr(PatternChangeIntervalMS);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternDeviation]:=IntToStr(PatternChangeDeviationPct);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFadeSteps]:=IntToStr(PatternChangeFadeSteps);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFadeStepsInterval]:=IntToStr(PatternChangeFadeStepIntervalMS);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternMorphStepsInterval]:=IntToStr(PatternChangePixelMorphIntervalMS);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineShowTitleOnStartup]:=BooleanText[ShowTitleOnStartup];
      end;

    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuTransparencyPct]:=IntToStr(Menu.TransparencyPct);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuTextShadow]:=BooleanText[Menu.TextShadow];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.MenuPanel.Color),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsFocusedEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsDisabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsTextShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.TextShadowColor[bsEnabled]),6));

    LoadFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontName);
    end;

  with Viewer1 do begin
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerFileName]:=FileName;
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerResizeToWindowWidth]:=Viewer1ResizeToWindowWidthText[Data.ResizeToWindowWidth];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowEnabled]:=BooleanText[Data.SlideShow];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeDelayMS]:=IntToStr(Data.TimeDelayMS);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeFadeMS]:=IntToStr(Data.TimeFadeMS);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowRandomOrder]:=BooleanText[Data.RandomOrder];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuTransparencyPct]:=IntToStr(Menu.TransparencyPct);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuTextShadow]:=BooleanText[Menu.TextShadow];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.MenuPanel.Color),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsFocusedEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsDisabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsTextShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.TextShadowColor[bsEnabled]),6));
    LoadFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontName);
    end;
{
  with Fireworks do begin
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsInterval]:=IntToStr(BuiltinfireworkTemplate[ftRocket1Color].Timer.Interval);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsDeviation]:=IntToStr(BuiltinfireworkTemplate[ftRocket1Color].Timer.IntervalDeviationPct);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsInterval]:=IntToStr(BuiltinfireworkTemplate[ftRocket2Colors].Timer.Interval);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsDeviation]:=IntToStr(BuiltinfireworkTemplate[ftRocket2Colors].Timer.IntervalDeviationPct);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuTransparencyPct]:=IntToStr(Menu.TransparencyPct);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuTextShadow]:=BooleanText[Menu.TextShadow];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.MenuPanel.Color),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsFocusedEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsDisabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsTextShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.TextShadowColor[bsEnabled]),6));
    with Menu.MenuPanel.Font do begin
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontName]:=Name;
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontSize]:=IntToStr(Size);
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontBold]:=BooleanText[fsBold in Style];
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontItalic]:=BooleanText[fsItalic in Style];
      SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontUnderlined]:=BooleanText[fsUnderline in Style];
      end;
    end;
}
  with Fractals do begin
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsPaletteFileName]:=PaletteFileName;
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsAntiAliasing]:=BooleanText[AntiAliasing];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuTransparencyPct]:=IntToStr(Menu.TransparencyPct);
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuTextShadow]:=BooleanText[Menu.TextShadow];
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.MenuPanel.Color),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsFocusedEnabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.ButtonFontColor[Menu_.bsDisabled]),6));
    SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsTextShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Menu.TextShadowColor[bsEnabled]),6));
    LoadFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontName);
    end;

  with MPlayerForm do with FormColors do begin
    SettingsString[stMusicPlayerOpenWindowColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsBackgroundText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundTextColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsHighlightedText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(HighlightedTextColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsButton    ]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonTextColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsFocusedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonTextColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsGrayedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonTextColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowTextColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsFocusedWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowColor),6));
    SettingsString[stMusicPlayerOpenWindowColorsFocusedWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowTextColor),6));
    end;
{$ENDIF}

  with MainForm.GamePictures.Pictures[ptScreenBackground] do begin
    SettingsString[stGraphicsBackgroundVisible]:=BooleanText[Visible];
    SettingsString[stGraphicsBackgroundImageFileName]:=FileName;
    SettingsString[stGraphicsBackgroundImageSectionLeft]:=IntToStr(SourceRect.Left);
    SettingsString[stGraphicsBackgroundImageSectionTop]:=IntToStr(SourceRect.Top);
    SettingsString[stGraphicsBackgroundImageSectionWidth]:=IntToStr(Max(0,SourceRect.Right-SourceRect.Left));
    SettingsString[stGraphicsBackgroundImageSectionHeight]:=IntToStr(Max(0,SourceRect.Bottom-SourceRect.Top));
    SettingsString[stGraphicsBackgroundImageView]:=ImageViewText[View];
    SettingsString[stGraphicsBackgroundImageAntialiasing]:=AntiAliasingText[Antialiasing];
    SettingsString[stGraphicsBackgroundColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
    end;

  with MainForm.GamePictures do begin
    for i:=Ord(ptBoardBackground) to Ord(ptReverseModeBackground) do with Pictures[GView_.TPictureType(i)] do begin
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  1)]:=BooleanText[Visible];
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  3)]:=FileName;
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  5)]:=IntToStr(SourceRect.Left);
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  6)]:=IntToStr(SourceRect.Top);
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  7)]:=IntToStr(Max(0,SourceRect.Right-SourceRect.Left));
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  8)]:=IntToStr(Max(0,SourceRect.Bottom-SourceRect.Top));
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+  9)]:=ImageViewText[View];
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 10)]:=AntiAliasingText[Antialiasing];
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 11)]:=RGB_BGR(Misc_.IntToHex(RGBToColor(MaskBitMapColor),6));
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 12)]:=IntToStr(MaskBitMapPct);
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 13)]:=BooleanText[Masked];
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 14)]:=BooleanText[Transparency[GView_.TPictureType(i)]];
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 15)]:=IntToStr(TransparencyPct[GView_.TPictureType(i)]);
        SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+ 16)]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
        end;

    with ReverseModePlayerStartPosition do begin
         SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionVisible   ]        := BooleanText[ Visible];
         SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridSize  ]        := IntToStr( GridSize );
         SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridColor ]        := RGB_BGR(Misc_.IntToHex(RGBToColor(GridColor),6));
         SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridShadowColor ]  := RGB_BGR(Misc_.IntToHex(RGBToColor(GridShadowColor),6));
         end;

    for p:=ptPlayer to ptBoxOnGoalAnimation do with Pictures[p] do begin
        j:=PictureSettingsOffset[p];
        SettingsString[TSettings(j+ 2)]:=FileName;
        SettingsString[TSettings(j+ 4)]:=IntToStr(SourceRect.Left);
        SettingsString[TSettings(j+ 5)]:=IntToStr(SourceRect.Top);
        SettingsString[TSettings(j+ 6)]:=IntToStr(Max(0,SourceRect.Right-SourceRect.Left));
        SettingsString[TSettings(j+ 7)]:=IntToStr(Max(0,SourceRect.Bottom-SourceRect.Top));
        SettingsString[TSettings(j+ 8)]:=BooleanText[Masked];
        SettingsString[TSettings(j+ 9)]:=RGB_BGR(Misc_.IntToHex(RGBToColor(MaskBitMapColor),6));
        SettingsString[TSettings(j+10)]:=IntToStr(MaskBitMapPct);
        SettingsString[TSettings(j+11)]:=BooleanText[MaskExteriorOnly];
        SettingsString[TSettings(j+12)]:=AntiAliasingText[AntiAliasing];
        if      p<ptPlayerAnimation then
                if   (p<>ptFloor) and (p<>ptGoal) then
                     SettingsString[TSettings(j+13)]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6))
                else SettingsString[TSettings(j+15)]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
        if      p=ptPlayerOnGoal then begin
                SettingsString[TSettings(j+14)]:=BooleanText[MainForm.GamePictures.UsePlayerImageForMoveAnimationAlsoForGoalSquares];
                SettingsString[TSettings(j+15)]:=BooleanText[MainForm.GamePictures.UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares];
                end
        else if p=ptBoxOnGoal then begin
                SettingsString[TSettings(j+14)]:=BooleanText[MainForm.GamePictures.UseBoxImageForMoveAnimationAlsoForGoalSquares];
                SettingsString[TSettings(j+15)]:=BooleanText[MainForm.GamePictures.UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares]
                end
        else if p=ptWall then begin
                SettingsString[TSettings(j+14)]:=WallTypeText[MainForm.GamePictures.WallType];
                SettingsString[TSettings(j+16)]:=IntToStr(MainForm.GamePictures.WallCap.X);
                SettingsString[TSettings(j+17)]:=IntToStr(MainForm.GamePictures.WallCap.Y);
                SettingsString[TSettings(j+19)]:=IntToStr(MainForm.GamePictures.OuterWallTrimming.Left);
                SettingsString[TSettings(j+20)]:=IntToStr(MainForm.GamePictures.OuterWallTrimming.Top);
                SettingsString[TSettings(j+21)]:=IntToStr(MainForm.GamePictures.OuterWallTrimming.Right);
                SettingsString[TSettings(j+22)]:=IntToStr(MainForm.GamePictures.OuterWallTrimming.Bottom);
                end
        else if (p=ptFloor) or (p=ptGoal) then begin
                SettingsString[TSettings(j+13)]:=BooleanText[MainForm.GamePictures.Transparency   [p]];
                SettingsString[TSettings(j+14)]:=IntToStr   (MainForm.GamePictures.TransparencyPct[p]);
                if p=ptFloor then SettingsString[TSettings(j+16)]:=BooleanText[Visible];
                end;
        if (p=ptPlayerAnimation) or (p=ptPlayerOnGoalAnimation) or
           (p=ptBoxAnimation   ) or (p=ptBoxOnGoalAnimation   ) then
           SettingsString[TSettings(j+13)]:=InBetweenFramesTypeText[MainForm.GamePictures.InBetweenFramesType[p]];
        end;

     SettingsString[stGraphicsBoardFiguresMaxZoomFactorPct]:=IntToStr(MainForm.GamePictures.MaxZoomFactorPct);
     SettingsString[stGraphicsBoardGridEnabled]:=BooleanText[MainForm.GamePictures.GridEnabled];
     SettingsString[stGraphicsBoardGridColor1]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GamePictures.GridColor1),6));
     SettingsString[stGraphicsBoardGridColor2]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.GamePictures.GridColor2),6));
    end;

  with MainForm.Menu do begin
    SettingsString[stGraphicsMenuTransparentButtons]:=BooleanText[Transparency];
    SettingsString[stGraphicsMenuTextShadow]:=BooleanText[TextShadow];
    for i:=Ord(Low(ButtonPicture)) to Ord(High(ButtonPicture)) do begin
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 2)]:=ButtonPicture[TButtonState(i)].FileName;
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 4)]:=IntToStr(TileRect[TButtonState(i)].Left);
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 5)]:=IntToStr(TileRect[TButtonState(i)].Top);
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 6)]:=IntToStr(Max(0,TileRect[TButtonState(i)].Right-TileRect[TButtonState(i)].Left));
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 7)]:=IntToStr(Max(0,TileRect[TButtonState(i)].Bottom-TileRect[TButtonState(i)].Top));
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 8)]:=RGB_BGR(Misc_.IntToHex(RGBToColor(ButtonPicture[TButtonState(i)].MaskBitMapColor),6));
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 9)]:=IntToStr(ButtonPicture[TButtonState(i)].MaskBitMapPct);
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+10)]:=BooleanText[ButtonPicture[TButtonState(i)].Masked];
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+11)]:=IntToStr(TransparencyPct[TButtonState(i)]);
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+12)]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonFontColor[TButtonState(i)]),6));
        SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+13)]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(TextShadowColor[TButtonState(i)]),6));
        end;
    SettingsString[stGraphicsMenuButtonsEdgesEdgeSmoothing]:=BooleanText[EdgeSmoothing];
    SettingsString[stGraphicsMenuButtonsEdgesTransparencyPct]:=IntToStr(EdgeTransparencyPct);
    LoadFont(MainForm.MenuPanel.Font,stGraphicsMenuButtonsFontName);
    end;

  with MainForm.RotateAndFlipPopupMenu do with FormColors do begin
    SettingsString[stGraphicsMenuPopupMenuColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsBackgroundText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundTextColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsHighlightedText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(HighlightedTextColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsButton    ]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonTextColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsFocusedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonTextColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsGrayedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonTextColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowTextColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsFocusedWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowColor),6));
    SettingsString[stGraphicsMenuPopupMenuColorsFocusedWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowTextColor),6));
    end;

  with MainForm.Status do begin
    SettingsString[stGraphicsStatusImageFileName]:=FileName;
    SettingsString[stGraphicsStatusImageSectionLeft]:=IntToStr(TileRect.Left);
    SettingsString[stGraphicsStatusImageSectionTop]:=IntToStr(TileRect.Top);
    SettingsString[stGraphicsStatusImageSectionWidth]:=IntToStr(Max(0,TileRect.Right-TileRect.Left));
    SettingsString[stGraphicsStatusImageSectionHeight]:=IntToStr(Max(0,TileRect.Bottom-TileRect.Top));
    SettingsString[stGraphicsStatusImageSectionBackgroundColor]:=RGB_BGR(Misc_.IntToHex(RGBToColor(MaskBitMapColor),6));
    SettingsString[stGraphicsStatusImageSectionBackgroundColorTolerancePct]:=IntToStr(MaskBitMapPct);
    SettingsString[stGraphicsStatusMasked]:=BooleanText[Masked];
    SettingsString[stGraphicsStatusTransparencyPct]:=IntToStr(TransparencyPct);
    SettingsString[stGraphicsStatusEdgesEdgeSmoothing]:=BooleanText[EdgeSmoothing];
    SettingsString[stGraphicsStatusEdgesTransparencyPct]:=IntToStr(EdgeTransparencyPct);
    SettingsString[stGraphicsStatusImageSlicesLeft]:=IntToStr(LeftSlice);
    SettingsString[stGraphicsStatusImageSlicesBody]:=IntToStr(BodySlice);
    SettingsString[stGraphicsStatusImageSlicesRight]:=IntToStr(RightSlice);
    SettingsString[stGraphicsStatusButtonTransparencyPct]:=IntToStr(ButtonTransparencyPct);
    SettingsString[stGraphicsStatusPanelsBoundsTop]:=IntToStr(PanelBoundsTop);
    SettingsString[stGraphicsStatusPanelsBoundsBottom]:=IntToStr(PanelBoundsBottom);
    SettingsString[stGraphicsStatusPanelsColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(PanelColor),6));
    SettingsString[stGraphicsStatusPanelsTransparencyPct]:=IntToStr(PanelTransparencyPct);
    SettingsString[stGraphicsStatusPanelsRoundingRectanglePixels]:=IntToStr(PanelRoundRect);
    LoadFont(MainForm.StatusPanel.Font,stGraphicsStatusFontName);
    SettingsString[stGraphicsStatusSubTextsAreTransparent]:=BooleanText[SubTextsAreTransparent];
    SettingsString[stGraphicsStatusSubTextsFontColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(SubTextsFontColor),6));
    SettingsString[stGraphicsStatusSubTextsFontSize]:=IntToStr(SubTextsFontSize);
    SettingsString[stGraphicsStatusTextShadow]:=BooleanText[TextShadow];
    SettingsString[stGraphicsStatusTextShadowColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(TextShadowColor),6));;
    end;

  with MainForm.MultiView do begin
    SettingsString[stGraphicsMultipleViewsColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor),6));
    SettingsString[stGraphicsMultipleViewsColorsFocusedBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedBackgroundColor),6));
    SettingsString[stGraphicsMultipleViewsColorsLine]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(LineColor),6));
    SettingsString[stGraphicsMultipleViewsColorsText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(TextColor),6));
    SettingsString[stGraphicsMultipleViewsColorsFocusedText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedTextColor),6));
    SettingsString[stGraphicsMultipleViewsColorsShadow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ShadowColor),6));
    SettingsString[stGraphicsMultipleViewsBackgroundTransparencyPct]:=IntToStr(BackgroundTransparencyPct);
    LoadFont(Font,stGraphicsMultipleViewsFontName);
    end;

  with ToolsForm do begin
    LoadFont(Font,stGraphicsToolsWindowFontName);
    if   ButtonsFileName='' then
         SettingsString[stGraphicsToolsButtonsImageFileName]:=DEFAULT_VALUE
    else SettingsString[stGraphicsToolsButtonsImageFileName]:=ButtonsFileName;

    with GameViewer.BackgroundPict do begin
      SettingsString[stGraphicsToolsBoardBackgroundVisible]:=BooleanText[Visible];
      if   FileName='' then
           SettingsString[stGraphicsToolsBoardBackgroundImageFileName]:=DEFAULT_VALUE
      else SettingsString[stGraphicsToolsBoardBackgroundImageFileName]:=FileName;
      SettingsString[stGraphicsToolsBoardBackgroundImageView]:=ImageViewText[View];
      SettingsString[stGraphicsToolsBoardBackgroundImageAntialiasing]:=AntiAliasingText[Antialiasing];
      SettingsString[stGraphicsToolsBoardBackgroundColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(Color),6));
      end;

    with GameViewer.SkinPict do begin
      if   FileName='' then
           SettingsString[stGraphicsToolsBoardSkinImageFileName]:=DEFAULT_VALUE
      else SettingsString[stGraphicsToolsBoardSkinImageFileName]:=FileName;
      SettingsString[stGraphicsToolsBoardSkinImageMasked]:=BooleanText[Masked];
      SettingsString[stGraphicsToolsBoardSkinImageBackgroundColor]:=RGB_BGR(Misc_.IntToHex(RGBToColor(MaskBitMapColor),6));
      SettingsString[stGraphicsToolsBoardSkinImageBackgroundColorTolerancePct]:=IntToStr(MaskBitMapPct);
      SettingsString[stGraphicsToolsBoardSkinImageAntialiasing]:=AntiAliasingText[AntiAliasing];
      end;

    with GameViewer do begin
      SettingsString[stGraphicsToolsBoardSquareSetSelectedSquaresColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(SquareSetSelectedSquaresColor),6));
      SettingsString[stGraphicsToolsBoardSquareSetNotSelectedSquaresColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(SquareSetNotSelectedSquaresColor),6));
      SettingsString[stGraphicsToolsBoardSquareSetTransparencyPct]:=IntToStr(SquareSetTransparencyPct);
      end;

    for i:=Ord(Low(Tools_.TEditorCursorType)) to Ord(High(Tools_.TEditorCursorType)) do with Editor.Cursors[Tools_.TEditorCursorType(i)] do begin
        SettingsString[TSettings(i*4+Ord(stGraphicsToolsEditorCursorsCellCursor)+1)]:=RGB_BGR(Misc_.IntToHex(PenColor,6));
        SettingsString[TSettings(i*4+Ord(stGraphicsToolsEditorCursorsCellCursor)+2)]:=IntToStr(PenWidth);
        SettingsString[TSettings(i*4+Ord(stGraphicsToolsEditorCursorsCellCursor)+3)]:=RGB_BGR(Misc_.IntToHex(ShadowColor,6));
        end;

    SettingsString[stGraphicsToolsEditorFrameColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GameViewer.FrameColor),6));
    SettingsString[stGraphicsToolsEditorFrameShadowColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GameViewer.FrameShadowColor),6));
    SettingsString[stGraphicsToolsEditorGridEnabled]:=BooleanText[ToolsForm.EditmenuItemGrid.Checked];
    SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsBackgroundColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ReplaySpeedTrackBarBackgroundColor),6));
    SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsSliderColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ReplaySpeedTrackBarSliderColor),6));
    SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsFontColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ReplaySpeedTrackBarFontColor),6));
    SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsShadowColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ReplaySpeedTrackBarShadowColor),6));
    end;

  with LevelSetForm do with FormColors do begin
    LoadFont(Font,stGraphicsToolsInternalClipboardWindowFontName);
    SettingsString[stGraphicsToolsInternalClipboardColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsBackgroundText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundTextColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsHighlightedText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(HighlightedTextColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsButton    ]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonTextColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsFocusedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonTextColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsGrayedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonTextColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowTextColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsFocusedWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowColor),6));
    SettingsString[stGraphicsToolsInternalClipboardColorsFocusedWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowTextColor),6));

    SettingsString[stGraphicsToolsInternalClipboardColorsNotesWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(InfoMemo.Color),6));
    SettingsString[stGraphicsToolsInternalClipboardTextsClipboardItemName]:=StandardClipboardItemName;
    LoadFont(InfoMemo.Font,stGraphicsToolsInternalClipboardEditFontName);
    end;

  with DuplicatesForm do begin
    SettingsString[stGraphicsToolsDuplicateLevelsColorsBackgroundColor1]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor1),6));
    SettingsString[stGraphicsToolsDuplicateLevelsColorsTextColor1]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(TextColor1),6));
    SettingsString[stGraphicsToolsDuplicateLevelsColorsBackgroundColor2]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor2),6));
    SettingsString[stGraphicsToolsDuplicateLevelsColorsTextColor2]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(TextColor2),6));
    SettingsString[stGraphicsToolsDuplicateLevelsColorsHighlightBackgroundColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(HighlightBackgroundColor),6));
    SettingsString[stGraphicsToolsDuplicateLevelsColorsHighlightTextColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(HighlightTextColor),6));
    end;

  with SnapshotsForm do with FormColors do begin
    LoadFont(Font,stGraphicsSnapshotsWindowFontName);
    SettingsString[stGraphicsSnapshotsColorsBackground]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundColor),6));
    SettingsString[stGraphicsSnapshotsColorsBackgroundText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(BackgroundTextColor),6));
    SettingsString[stGraphicsSnapshotsColorsHighlightedText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(HighlightedTextColor),6));
    SettingsString[stGraphicsSnapshotsColorsButton    ]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonColor),6));
    SettingsString[stGraphicsSnapshotsColorsButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ButtonTextColor),6));
    SettingsString[stGraphicsSnapshotsColorsFocusedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonColor),6));
    SettingsString[stGraphicsSnapshotsColorsFocusedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedButtonTextColor),6));
    SettingsString[stGraphicsSnapshotsColorsGrayedButton]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonColor),6));
    SettingsString[stGraphicsSnapshotsColorsGrayedButtonText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(GrayedButtonTextColor),6));
    SettingsString[stGraphicsSnapshotsColorsWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowColor),6));
    SettingsString[stGraphicsSnapshotsColorsWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(WindowTextColor),6));
    SettingsString[stGraphicsSnapshotsColorsFocusedWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowColor),6));
    SettingsString[stGraphicsSnapshotsColorsFocusedWindowText]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(FocusedWindowTextColor),6));

    SettingsString[stGraphicsSnapshotsColorsNotesWindow]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(InfoMemo.Color),6));
    SettingsString[stGraphicsSnapshotsTextsSolution]:=SolutionName;
    SettingsString[stGraphicsSnapshotsTextsBestSolution]:=BestSolutionName;
    SettingsString[stGraphicsSnapshotsTextsBestSolutionMoves]:=BestSolutionMovesName;
    SettingsString[stGraphicsSnapshotsTextsBestSolutionPushes]:=BestSolutionPushesName;
    SettingsString[stGraphicsSnapshotsTextsNormalModeSnapshotName]:=NormalModeSnapshotName;
    SettingsString[stGraphicsSnapshotsTextsReverseModeSnapshotName]:=ReverseModeSnapshotName;
    LoadFont(InfoMemo.Font,stGraphicsSnapshotsEditFontName);
    end;

  with OpenForm do begin
    LoadFont(Font,stGraphicsOpenWindowFontName);
    SettingsString[stGraphicsOpenWindowColorsCollectionBackgroundColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CollectionStringGrid.Color),6));
    SettingsString[stGraphicsOpenWindowColorsCollectionTextColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CollectionStringGrid.Font.Color),6));
//  SettingsString[stGraphicsOpenWindowColorsCollectionHighlightBackgroundColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CollectionHighlightBackgroundColor),6));
//  SettingsString[stGraphicsOpenWindowColorsCollectionHighlightTextColor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CollectionHighlightTextColor),6));
    end;

  with OptionsForm do begin
    LoadFont(Font,stGraphicsOptionsWindowFontName);
    end;

  with DuplicatesForm do begin
    LoadFont(Font,stGraphicsDuplicatesWindowFontName);
    end;

  with HelpForm do begin
    LoadFont(Font,stGraphicsHelpWindowFontName);
    end;

//if EditForm<>nil then with EditForm do begin
//   SettingsString[stGraphicsEditorColorsWall]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.Game.Color[WallColor]),6));
//   SettingsString[stGraphicsEditorColorsFloor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.Game.Color[FloorColor]),6));
//   SettingsString[stGraphicsEditorColorsBox]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.Game.Color[BoxColor]),6));
//   SettingsString[stGraphicsEditorColorsGoal]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.Game.Color[GoalColor]),6));
//   SettingsString[stGraphicsEditorColorsPlayer]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(MainForm.Game.Color[PlayerColor]),6));
//   SettingsString[stGraphicsEditorColorsCursor]:=RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(CursorColor),6));
//   end;

  Modified:=False;
  TreeToGrid;
  ShowStatus;
end;

procedure TOptionsForm.SaveAsDefaultValues;
var i:Integer;
begin
  for i:=Ord(Low(TSettings)) to Ord(High(TSettings)) do begin
      DefaultSettingsString[TSettings(i)]:=SettingsString[TSettings(i)];
      if   SettingsEditor[TSettings(i)]=seNone then //TreeView1.Items[i].HasChildren then
           DefaultSettingsString[TSettings(i)]:=STAR // non-empty = flag for 'ok' even when it's a header
      else DefaultSettingsString[TSettings(i)]:=SettingsString[TSettings(i)];
      end;
//DefaultSettingsString[stAssociateFileTypes]:=BooleanText[False];
end;

procedure TOptionsForm.SaveData( UpdateWindowSizes__ : Boolean );
var i,j,k:Integer; b:Menu_.TButtonState; p:TPictureType; R:TRect;

  procedure SaveFont(Font:TFont; Index:TSettings);
  var i:Integer;
  begin
    with Font do begin
      Name:=SettingsString[Index];
      Size:=StrToInt(SettingsString[Succ(Index)]);
      if   SafeStrToInt(RGB_BGR(SettingsString[Succ(Succ(Index))]),True,i) then
           Color:=TColor(i);
      if   StrToBool(SettingsString[Succ(Succ(Succ(Index)))]) then
           Style:=Style+[fsBold]
      else Style:=Style-[fsBold];
      if   StrToBool(SettingsString[Succ(Succ(Succ(Succ(Index))))]) then
           Style:=Style+[fsItalic]
      else Style:=Style-[fsItalic];
      if   StrToBool(SettingsString[Succ(Succ(Succ(Succ(Succ(Index)))))]) then
           Style:=Style+[fsUnderline]
      else Style:=Style-[fsUnderLine];
      end;
  end;

begin // SaveData
  //SaveToFile(MainForm.ApplicationDataPath+'t1.set');

  MainForm.Game.MoveAnimationEnabled:=StrToBool(SettingsString[stMoveAnimationEnabled]);
  MainForm.Game.SmoothMoveAnimationEnabled:=StrToBool(SettingsString[stSmoothMoveAnimationEnabled]);
  MainForm.Game.SessionSmoothMoveAnimationEnabled:=MainForm.Game.SmoothMoveAnimationEnabled;
  MainForm.Game.PlayerDirectionAnimationEnabled:=StrToBool(SettingsString[stPlayerDirectionAnimationEnabled]);
//MainForm.Game.SmoothMoveAnimationThresholdEnabled:=StrToBool(SettingsString[stSmoothMoveAnimationThresholdEnabled]);
//MainForm.Game.SmoothMoveAnimationThresholdMaxPixelsPerMove:=StrToInt(SettingsString[stSmoothMoveAnimationThresholdMaxPixelsPerMove]);
  MainForm.Game.AnimateDoMoveMS:=StrToInt(SettingsString[stMoveAnimationDoMove]);
  MainForm.Game.AnimateUndoMoveMS:=StrToInt(SettingsString[stMoveAnimationUndoMove]);
  MainForm.Game.AnimateReplayMovesMS:=StrToInt(SettingsString[stMoveAnimationReplayMoves]);
  MainForm.Game.AnimateMovesOnMouseWheelUpDown:=StrToBool(SettingsString[stMoveAnimationEnabledOnMouseWheelUpDown]);
  MainForm.Game.ForkliftDrivingEnabled:=StrToBool(SettingsString[stMoveAnimationForkliftDrivingEnabled]);
  MainForm.Game.ForkliftDrivingDriveInReverseSquares:=StrToInt(SettingsString[stMoveAnimationForkliftDrivingDriveInReverseSquares]);

  MainForm.Game.PathFindingMaxTimeMS:=StrToInt(SettingsString[stPathFindingMaxTime]);
  MainForm.Deadlocks.CalculatePushesLowerBoundTimeLimitMS:=StrToInt(SettingsString[stPathFindingPushesLowerBoundCalculationMaxTime]);
  MainForm.Game.PathFindingOptimizeMoves:=AnsiCompareText(SettingsString[stPathFindingOptimizeMovesOrPushes],PathFindingOptimizationTypeText[True])=0;
  MainForm.GameViewer.LegalMovesInfo.Enabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesEnabled]);
  MainForm.GameViewer.LegalMovesInfo.Size:=StrToInt(SettingsString[stPathFindingShowLegalMovesSize]);
  MainForm.GameViewer.LegalMovesInfo.TransparentImage:=StrToBool(SettingsString[stPathFindingShowLegalMovesTransparentImage]);
  MainForm.GameViewer.LegalMovesInfo.Transparency:=StrToInt(SettingsString[stPathFindingShowLegalMovesTransparency]);
  MainForm.GameViewer.LegalMovesInfo.BoxCursor.Enabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesBoxCursorEnabled]);
  MainForm.GameViewer.LegalMovesInfo.PlayerCursor.Enabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesPlayerCursorEnabled]);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesPlayerCursorColor]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.PlayerCursor.Color:=TColor(i);
  MainForm.GameViewer.LegalMovesInfo.PlayerCursor.PenWidth:=StrToInt(SettingsString[stPathFindingShowLegalMovesPlayerCursorPenWidth]);
  MainForm.GameViewer.LegalMovesInfo.PlayerCursor.Size:=StrToInt(SettingsString[stPathFindingShowLegalMovesPlayerCursorSize]);
  MainForm.GameViewer.LegalMovesInfo.PlayerCursor.ShadowEnabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesPlayerCursorShadowEnabled]);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesPlayerCursorShadowColor]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.PlayerCursor.ShadowColor:=TColor(i);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesBoxCursorColor]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.BoxCursor.Color:=TColor(i);
  MainForm.GameViewer.LegalMovesInfo.BoxCursor.PenWidth:=StrToInt(SettingsString[stPathFindingShowLegalMovesBoxCursorPenWidth]);
  MainForm.GameViewer.LegalMovesInfo.BoxCursor.Size:=StrToInt(SettingsString[stPathFindingShowLegalMovesBoxCursorSize]);
  MainForm.GameViewer.LegalMovesInfo.BoxCursor.ShadowEnabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesBoxCursorShadowEnabled]);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesBoxCursorShadowColor]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.BoxCursor.ShadowColor:=TColor(i);
  MainForm.GameViewer.LegalMovesInfo.DeadlocksEnabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesDeadlocksEnabled]);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesDeadlocksColor]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.DeadlocksColor:=TColor(i);
  MainForm.GameViewer.LegalMovesInfo.JumpMovesEnabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesJumpMovesEnabled]);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesJumpMovesColorsBackground]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.JumpMovesBackgroundColor:=TColor(i);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesJumpMovesColorsText]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.JumpMovesTextColor:=TColor(i);
  if SafeStrToInt(RGB_BGR(SettingsString[stPathFindingShowLegalMovesJumpMovesColorsTextShadow]),True,i) then
     MainForm.GameViewer.LegalMovesInfo.JumpMovesTextShadowColor:=TColor(i);
  MainForm.GameViewer.LegalMovesInfo.PlayerAnimationEnabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesAnimationPlayerAnimationEnabled]);
  MainForm.GameViewer.LegalMovesInfo.BoxAnimationEnabled:=StrToBool(SettingsString[stPathFindingShowLegalMovesAnimationBoxAnimationEnabled]);
  MainForm.ShowBoardCoordinates:=StrToBool(SettingsString[stPathFindingShowBoardCoordinates]);
  if BoardGraphicsModified then MainForm.GameViewer.Modified:=True;

  MainForm.ShowSolutionMoves:=AnsiCompareText(SettingsString[stSolutionsPreferredType],SolutionTypeText[True])=0;
  MainForm.Game.SaveBestSolutionsAutomatically:=StrToBool(SettingsString[stSolutionsSaveAutomatically]);
  MainForm.Game.SaveOldSolutionsAfterFindingBetterOnes:=StrToBool(SettingsString[stSolutionsSaveOldSolutionsAfterFindingBetterOnes]);
  MainForm.Game.SaveOldSolutionsAfterClipboardImport:=StrToBool(SettingsString[stSolutionsSaveOldSolutionsAfterClipboardImport]);
  MainForm.Game.SolutionsRequireAtLeastOnePush:=StrToBool(SettingsString[stSolutionsRequireAtLeastOnePush]);
  MainForm.GameViewer.SolutionsInfo.PlayerAnimationEnabled:=StrToBool(SettingsString[stSolutionsAnimationPlayerAnimationEnabled]);
  MainForm.GameViewer.SolutionsInfo.BoxAnimationEnabled:=StrToBool(SettingsString[stSolutionsAnimationBoxAnimationEnabled]);
//SokGame_.ScoreMetricsWeights.BoxLines:=StrToInt(SettingsString[stSolutionsScoreMetricsWeightsBoxLines]);
//SokGame_.ScoreMetricsWeights.BoxChanges:=StrToInt(SettingsString[stSolutionsScoreMetricsWeightsBoxChanges]);
//SokGame_.ScoreMetricsWeights.PushingSessions:=StrToInt(SettingsString[stSolutionsScoreMetricsWeightsPushingSessions]);
  MainForm.SecondaryMetricsInSolutionButtonHint:=StrToBool(SettingsString[stSolutionsSecondaryMetricsInSolutionButtonHint]);
  SnapshotsForm.AllowDeletionOfBestSolutions:=StrToBool(SettingsString[stSolutionsAllowDeletionOfBestSolutionsInSnapshotsWindow]);
  MainForm.ErrorRecoveryEnabled:=StrToBool(SettingsString[stSolutionsErrorRecoveryEnabled]);
  MainForm.Game.CalculateScoreAndState; {the game state may have changed as a consequence of the option 'Solutions require at least one push'}

  MainForm.Game.SaveSnapshotsAutomatically:=StrToBool(SettingsString[stSnapshotsSaveAutomatically]);
  MainForm.Game.SecondaryMetricsInTitles  :=StrToBool(SettingsString[stSnapshotsSecondaryMetricsInTitles]);

  MainForm.Game.ShowBoxStartPositionsAsGoalsInReverseMode:=StrToBool(SettingsString[stReverseModeShowBoxStartPositionsAsGoals]);
  MainForm.Game.JumpsAllowedAfterFirstBoxMoveInReverseMode:=StrToBool(SettingsString[stReverseModeJumpsAllowedAfterFirstBoxMove]);
  MainForm.Game.SetReverseMode(MainForm.Game.ReverseMode); //  update target-masks

  MainForm.Game.DeadlockDetection.Enabled:=StrToBool(SettingsString[stDeadlockDetectionEnabled]);
  i:=StrIndexOfCI(FSettingsString[stDeadlockDetectionType],DeadlockDetectionTypeText);
  if i>=0 then MainForm.Game.DeadlockDetection.DeadlockDetectionType:=TLowMediumHigh(i);
  MainForm.Game.DeadlockDetection.BlockMoves:=StrToBool(SettingsString[stDeadlockDetectionBlockMoves]);
  MainForm.Game.DeadlockDetection.LogEnabled:=StrToBool(SettingsString[stDeadlockDetectionLogEnabled]);
  if   MainForm.Game.DeadlockDetection.Enabled and MainForm.Game.DeadlockDetection.BlockMoves then
       MainForm.Game.SimpleIllegalMovesMask:=SIMPLE_ILLEGAL_MOVES_MASK
  else MainForm.Game.SimpleIllegalMovesMask:=0;
  MainForm.ShowSimpleDeadSquaresEnabled:=StrToBool(SettingsString[stDeadlockDetectionShowSimpleDeadSquaresEnabled]);
  if SafeStrToInt(RGB_BGR(SettingsString[stDeadlockDetectionShowSimpleDeadSquaresColor]),True,i) then
     MainForm.ShowSimpleDeadSquaresColor:=TColor(i);
  MainForm.ShowSimpleDeadSquaresTransparencyPct:=StrToInt(SettingsString[stDeadlockDetectionShowSimpleDeadSquaresTransparencyPct]);

  MainForm.UndoRedoCombinedMoves:=StrToBool(SettingsString[stUndoRedoCombinedMoves]);
  MainForm.OptimizeMovesBetweenPushes:=StrToBool(SettingsString[stOptimizeMovesBetweenPushes]);
  i:=StrIndexOfCI(FSettingsString[stOnLeftClickEmptySquareAction],OnLeftClickEmptySquareActionText);
  if i>=0 then MainForm.OnLeftClickEmptySquareAction:=TOnLeftClickEmptySquareAction(i);
  i:=StrIndexOfCI(FSettingsString[stOnRightClickAction],OnRightClickActionText);
  if i>=0 then MainForm.OnRightClickAction:=TOnRightClickAction(i);
  i:=StrIndexOfCI(FSettingsString[stOnShiftKeyAction],OnShiftKeyActionText);
  if i>=0 then MainForm.OnShiftKeyAction:=TOnShiftKeyAction(i);

  MainForm.Game.TimingEnabled:=StrToBool(SettingsString[stTimingEnabled]);
  MainForm.Game.TimingIdleTimeThresholdEnabled:=StrToBool(SettingsString[stTimingIdleTimeThresholdEnabled]);
  MainForm.Game.TimingIdleTimeThresholdMS:=1000*StrToInt(SettingsString[stTimingIdleTimeThresholdSeconds]);

  if UpdateWindowSizes__ then begin
     R.Left  :=StrToInt(SettingsString[stWindowsMainLeft]);
     R.Top   :=StrToInt(SettingsString[stWindowsMainTop]);
     R.Right :=R.Left+StrToInt(SettingsString[stWindowsMainWidth]);
     R.Bottom:=R.Top +StrToInt(SettingsString[stWindowsMainHeight]);
     if (R.Left       <>MainForm.Left)   or
        (R.Top        <>MainForm.Top)    or
        (RectWidth(R) <>MainForm.Width)  or
        (RectHeight(R)<>MainForm.Height) then begin
        if MainForm.WindowState<>wsNormal then MainForm.WindowState:=wsNormal; // restore normal window before changing size
        MainForm.SetBounds(R.Left,R.Top,RectWidth(R),RectHeight(R));
        end;
     end;
  MainForm.BoardDimensionsInTitleLine:=StrToBool(SettingsString[stWindowsMainBoardDimensionsInTitleLine]);
  MainForm.CollectionNameInTitleLine:=StrToBool(SettingsString[stWindowsMainCollectionNameInTitleLine]);

  i:=StrIndexOfCI(FSettingsString[stSelectionCursorType],CursorTypeText);
  if i>=0 then MainForm.SelectionCursorType:=TCursorType(i);
  i:=StrIndexOfCI(FSettingsString[stDragCursorType],CursorTypeText);
  if i>=0 then MainForm.DragCursorType:=TCursorType(i);
  i:=StrIndexOfCI(FSettingsString[stTrackingCursorType],CursorTypeText);
  if i>=0 then MainForm.TrackingCursorType:=TCursorType(i);

  MainForm.Solver.PluginFileName:=SettingsString[stToolsSolverFileName];
  i:=StrIndexOfCI(FSettingsString[stToolsSolverPriority],ThreadPriorityText);
  if i>=0 then MainForm.Solver.Priority:=TThreadPriority(i);
  MainForm.Solver.AddPluginNameToSolutionTitles:=StrToBool(SettingsString[stToolsSolverAddPluginNameToSolutionTitles]);
  MainForm.Solver.AddDateToSolutionNotes:=StrToBool(SettingsString[stToolsSolverAddDateToSolutionNotes]);
  MainForm.Solver.StatisticsEnabled:=StrToBool(SettingsString[stToolsSolverStatisticsEnabled]);
  MainForm.Solver.TimeLimitEnabled:=StrToBool(SettingsString[stToolsSolverTimeLimitEnabled]);
  i:=StrToInt(SettingsString[stToolsSolverTimeLimitSeconds]);
  if (i>=0) and (i<=MAX_PLUGIN_TIME_LIMIT_MS div ONE_THOUSAND) then MainForm.Solver.TimeLimitMS:=i*ONE_THOUSAND;

  MainForm.Optimizer.PluginFileName:=SettingsString[stToolsOptimizerFileName];
  i:=StrIndexOfCI(FSettingsString[stToolsOptimizerPriority],ThreadPriorityText);
  if i>=0 then MainForm.Optimizer.Priority:=TThreadPriority(i);
  MainForm.Optimizer.AddPluginNameToSolutionTitles:=StrToBool(SettingsString[stToolsOptimizerAddPluginNameToSolutionTitles]);
  MainForm.Optimizer.AddDateToSolutionNotes:=StrToBool(SettingsString[stToolsOptimizerAddDateToSolutionNotes]);
  MainForm.Optimizer.StatisticsEnabled:=StrToBool(SettingsString[stToolsOptimizerStatisticsEnabled]);
  MainForm.Optimizer.DiscardDethronedSolutions:=StrToBool(SettingsString[stToolsOptimizerDiscardDethronedSolutions]);
  MainForm.Optimizer.SeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly:=StrToBool(SettingsString[stToolsOptimizerSeparateBestMovesAndBestSolutionsInitializeOptimizationAccordingly]);
  MainForm.Optimizer.TimeLimitEnabled:=StrToBool(SettingsString[stToolsOptimizerTimeLimitEnabled]);
  i:=StrToInt(SettingsString[stToolsOptimizerTimeLimitSeconds]);
  if (i>=0) and (i<=MAX_PLUGIN_TIME_LIMIT_MS div ONE_THOUSAND) then MainForm.Optimizer.TimeLimitMS:=i*ONE_THOUSAND;
  ToolsForm.SettingsMenuAlternatingOptimizationsEnabled.Checked:=StrToBool(SettingsString[stToolsOptimizerAlternatingOptimizationsEnabled]);
  ToolsForm.SettingsMenuAlternatingOptimizationsEnabledClick(nil);
  i:=StrIndexOfCI(FSettingsString[stToolsOptimizerAlternatingOptimizationsType],AlternatingOptimizationsText);
  if (i>=Ord(Low(ToolsForm.AlternatingOptimizations))) and (i<=Ord(High(ToolsForm.AlternatingOptimizations))) then
     ToolsForm.AlternatingOptimizations:=TAlternatingOptimizations(i);
  i:=StrIndexOfCI(FSettingsString[stToolsOptimizerAlternatingOptimizationsTrigger],AlternatingOptimizationsTriggerTypeText);
  if (i>=Ord(Low(Boolean))) and (i<=Ord(High(Boolean))) then begin
     ToolsForm.SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce.Checked:=i=Ord(False);
     ToolsForm.SettingsMenuAlternatingOptimizationsTriggerOnImprovements.Checked:=not ToolsForm.SettingsMenuAlternatingOptimizationsTriggerAtLeastOnce.Checked;
     end;
  ToolsForm.SettingsMenuAlternatingOptimizationsRepeat.Checked:=StrToBool(SettingsString[stToolsOptimizerAlternatingOptimizationsRepeatEnabled]);
  ToolsForm.SettingsMenuAlternatingOptimizationsRepeatClick (nil);
  i:=StrIndexOfCI(FSettingsString[stToolsOptimizerAlternatingOptimizationsRepeatType],AlternatingOptimizationsRepeatTypeText);
  if (i>=Ord(Low(Boolean))) and (i<=Ord(High(Boolean))) then begin
     ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics.Checked:=i=Ord(False);
     ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnMovesPushesBoxLines.Checked:=not ToolsForm.SettingsMenuAlternatingOptimizationsRepeatOnAnyMetrics.Checked;
     end;
  MainForm.Optimizer.WallifyBoxesOutsideRangeOfPushesToBeOptimized:=StrToBool(SettingsString[stToolsOptimizerIntervalsWallifyBoxesOutsideRangeOfPushes]);
  MainForm.Optimizer.SubIntervalOverlapPct:=StrToInt(SettingsString[stToolsOptimizerIntervalsSubIntervalOverlapPct]);

  i:=StrIndexOfCI(FSettingsString[stToolsGeneratorPriority],ThreadPriorityText);
  if (i>=0) and (i<GeneratorForm.ComboBoxThreadPriority.Items.Count) then begin
     MainForm.Generator.Priority:=TThreadPriority(i);
     GeneratorForm.ComboBoxThreadPriority.ItemIndex:=i;
     end;

  DuplicatesForm.AdvancedIdentityCheck:=StrToBool(SettingsString[stToolsDuplicateFinderAdvancedIdentityCheck]);
  DuplicatesForm.MatchThresholdPercent:=StrToInt(SettingsString[stToolsDuplicateFinderCurrentLevelMatchThresholdPercent]);

  i:=StrIndexOfCI(FSettingsString[stToolsSkinExportBaseFormatType],SkinExportFormatTypeText);
  if i>=0 then SkinExportFormatType:=TSkinExportFormatType(i);

  MainForm.AddFileFormatDescriptionToFiles:=StrToBool(SettingsString[stAddFileFormatDescriptionToFiles]);
  if MainForm.SokoFile<>nil then
     MainForm.SokoFile.AddFileFormatDescriptionToFiles:=MainForm.AddFileFormatDescriptionToFiles;
  MainForm.Game.PrettyPrintGamesEnabled:=StrToBool(SettingsString[stPrettyPrintGamesEnabled]);
  MainForm.CopyLevelToClipboardFillFloors:=StrToBool(SettingsString[stCopyLevelToClipboardFillFloors]);
  i:=StrIndexOfCI(FSettingsString[stCopyLevelToClipboardFloorFillCharacter],Text_.FloorFillCharacter);
  if (i>=0) and (Length(FSettingsString[stCopyLevelToClipboardFloorFillCharacter])>=3) then MainForm.CopyLevelToClipboardFloorFillCharacter:=StrWithoutDoubleQuotes(SettingsString[stCopyLevelToClipboardFloorFillCharacter])[1];
  MainForm.CopyLevelCollectionToClipboardIncludeLevelComments:=StrToBool(SettingsString[stCopyLevelCollectionToClipboardIncludeLevelComments]);
  MainForm.CopyLevelToClipboardPreserveCombinedMoves:=StrToBool(SettingsString[stCopyLevelToClipboardPreserveCombinedMoves]);
  MainForm.CopyLevelToClipboardBasedOnSolutionSliceIncludeTitles:=StrToBool(SettingsString[stCopyLevelToClipboardBasedOnSolutionSliceIncludeTitles]);
  i:=StrIndexOfCI(FSettingsString[stRunLengthEncodingFloor],Text_.FloorFillCharacter);
  if (i>=0) and (Length(FSettingsString[stRunLengthEncodingFloor])>=3) then MainForm.RunLengthEncodingFloor:=StrWithoutDoubleQuotes(SettingsString[stRunLengthEncodingFloor])[1];
{
  OldHasFileAssociations:=MainForm.FileAssoc.HasFileAssociations;
  if StrToBool(SettingsString[stAssociateFileTypes]) <> OldHasFileAssociations then
     if   OldHasFileAssociations then
          MainForm.FileAssoc.UnAssociateFileTypes(True)
     else MainForm.FileAssoc.AssociateFileTypes(True);
}
  MainForm.Game.RestoreSaveGame:=StrToBool(SettingsString[stRestoreSaveGameOnOpen]);
  MainForm.AutoAdvanceWhenSolved:=StrToBool(SettingsString[stAutoAdvanceWhenSolved]);
  MainForm.MouseTrackingModeEnabled:=StrToBool(SettingsString[stMouseTrackingModeEnabled]);
  i:=StrIndexOfCI(FSettingsString[stBoardDimensionsAsText],Text_.BoardDimensionsAsText);
  if i>=0 then MainForm.BoardDimensionsAsText:=TBoardDimensionsAsText(i);
  MainForm.BoardDimensionsWithFloorCount:=StrToBool(SettingsString[stBoardDimensionsWithFloorCount]);
  i:=StrIndexOfCI(FSettingsString[stOnEscapeKeyAction],OnEscapeKeyActionText);
  if i>=0 then MainForm.OnEscapeKeyAction:=TOnEscapeKeyAction(i);
  i:=StrIndexOfCI(FSettingsString[stDisplayDriverChangedAction],DisplayDriverChangedActionText);
  if i>=0 then MainForm.DisplayDriverChangedAction:=TDisplayDriverChangedAction(i);
  MainForm.ConfirmRunningMultipleInstances:=StrToBool(SettingsString[stConfirmRunningMultipleInstances]);
  MainForm.ScreenSaverEnabled:=StrToBool(SettingsString[stScreenSaverEnabled]);
  MainForm.UseOnlyOneProcessor:=StrToBool(SettingsString[stUseOnlyOneProcessor]);

  if not OpenForm.Visible then begin
     OpenForm.Task:=otNone; // in order to trigger an 'OpenForm.FormResize' in 'OpenForm.InitTask' next time it's activated
     if UpdateWindowSizes__ then begin
        R.Left  :=StrToInt(SettingsString[stWindowsOpenLeft]);
        R.Top   :=StrToInt(SettingsString[stWindowsOpenTop]);
        R.Right :=R.Left+StrToInt(SettingsString[stWindowsOpenWidth]);
        R.Bottom:=R.Top +StrToInt(SettingsString[stWindowsOpenHeight]);
        if (R.Left       <>OpenForm.Left)   or
           (R.Top        <>OpenForm.Top)    or
           (RectWidth(R) <>OpenForm.Width)  or
           (RectHeight(R)<>OpenForm.Height) then begin
           if OpenForm.WindowState<>wsNormal then begin
              OpenForm.DoMaximize:=True; // 'kludge: avoid that the statement 'OpenForm.SetBounds()' below updates the form when it triggers 'OpenForm.FormResize'
              OpenForm.Task:=otNone; // ensure that 'OpenForm' calls 'FormResize' next time it's opened
              OpenForm.WindowState:=wsNormal; // restore normal window before changing size
              end;
           OpenForm.SetBounds(R.Left,R.Top,Max( RectWidth(R),OpenForm.CalculateMinWidth),Max(RectHeight(R),OpenForm.CalculateMinHeight));
           end;
        end;
     OpenForm.DiskGroupBox.Width :=StrToInt(SettingsString[stWindowsOpenDiskGroupBoxWidth ]);
     OpenForm.DiskGroupBox.Height:=StrToInt(SettingsString[stWindowsOpenDiskGroupBoxHeight]);

     i:=OpenForm.FileListBox1.Left-(OpenForm.DirectoryListBox1.Left+OpenForm.DirectoryListBox1.Width); // gap
     OpenForm.DirectoryListBox1.Width:=Max(OpenForm.FolderListBoxMinimumWidth,Min(StrToInt(SettingsString[stWindowsOpenFolderListBoxWidth]),OpenForm.DiskGroupBox.ClientWidth-OpenForm.FolderListBoxMinimumWidth-i-(2*OpenForm.DirectoryListBox1.Left)));
     OpenForm.FileListBox1.Left:=OpenForm.DirectoryListBox1.Left+OpenForm.DirectoryListBox1.Width+i;

     i:=OpenForm.GameDataGroupBox.Top-(OpenForm.GameBoardPanel.Top+OpenForm.GameBoardPanel.Height); // gap
     OpenForm.GameBoardPanel.Height:=Max(StrToInt(SettingsString[stWindowsOpenLevelPreviewHeight]),OpenForm.DiskGroupBoxMinimumHeight-OpenForm.GameBoardPanel.Top);
     OpenForm.GameDataGroupBox.Top:=OpenForm.GameBoardPanel.Top+OpenForm.GameBoardPanel.Height+i;
     end;

  OpenForm.SolutionColumns:=StrToInt(SettingsString[stWindowsOpenSolutionColumns]);
  OpenForm.MenuItemIncludeBuiltinSolutions.Checked:=StrToBool(SettingsString[stWindowsOpenIncludeBuiltinSolutions]);

  OpenForm.ToolTips.Enabled:=StrToBool(SettingsString[stWindowsOpenToolTipsEnabled]);
  OpenForm.ToolTips.OffsetX:=StrToInt (SettingsString[stWindowsOpenToolTipsOffsetX]);
  OpenForm.ToolTips.OffsetY:=StrToInt (SettingsString[stWindowsOpenToolTipsOffsetY]);

  if UpdateWindowSizes__ then begin
     R.Left  :=StrToInt(SettingsString[stWindowsToolsLeft]);
     R.Top   :=StrToInt(SettingsString[stWindowsToolsTop]);
     R.Right :=R.Left+StrToInt(SettingsString[stWindowsToolsWidth]);
     R.Bottom:=R.Top +StrToInt(SettingsString[stWindowsToolsHeight]);
     if (R.Left       <>ToolsForm.Left)   or
        (R.Top        <>ToolsForm.Top)    or
        (RectWidth(R) <>ToolsForm.Width)  or
        (RectHeight(R)<>ToolsForm.Height) then begin
        if ToolsForm.WindowState<>wsNormal then ToolsForm.WindowState:=wsNormal; // restore normal window before changing size
        ToolsForm.SetBounds(R.Left,R.Top,RectWidth(R),RectHeight(R));
        end;

     R.Left  :=StrToInt(SettingsString[stWindowsSnapshotsLeft]);
     R.Top   :=StrToInt(SettingsString[stWindowsSnapshotsTop]);
     R.Right :=R.Left+StrToInt(SettingsString[stWindowsSnapshotsWidth]);
     R.Bottom:=R.Top +StrToInt(SettingsString[stWindowsSnapshotsHeight]);
     if (R.Left       <>SnapshotsForm.Left)   or
        (R.Top        <>SnapshotsForm.Top)    or
        (RectWidth(R) <>SnapshotsForm.Width)  or
        (RectHeight(R)<>SnapshotsForm.Height) then begin
        if SnapshotsForm.WindowState<>wsNormal then SnapshotsForm.WindowState:=wsNormal; // restore normal window before changing size
        SnapshotsForm.SetBounds(R.Left,R.Top,RectWidth(R),RectHeight(R));
        end;

     R.Left  :=StrToInt(SettingsString[stWindowsInternalClipboardLeft]);
     R.Top   :=StrToInt(SettingsString[stWindowsInternalClipboardTop]);
     R.Right :=R.Left+StrToInt(SettingsString[stWindowsInternalClipboardWidth]);
     R.Bottom:=R.Top +StrToInt(SettingsString[stWindowsInternalClipboardHeight]);
     if (R.Left       <>LevelSetForm.Left)   or
        (R.Top        <>LevelSetForm.Top)    or
        (RectWidth(R) <>LevelSetForm.Width)  or
        (RectHeight(R)<>LevelSetForm.Height) then begin
        if LevelSetForm.WindowState<>wsNormal then LevelSetForm.WindowState:=wsNormal; // restore normal window before changing size
        LevelSetForm.SetBounds(R.Left,R.Top,RectWidth(R),RectHeight(R));
        end;
     end;

  MainForm.Sound.Enabled:=StrToBool(SettingsString[stSoundEnabled]);
  MainForm.Music.Volume64Kibi:=((StrToInt(SettingsString[stSoundVolume])*65535)+50) div 100;
  MainForm.Sound.ResetSoundEffectsOnLoadingASkin:=StrToBool(SettingsString[stSoundResetSoundEffectsOnLoadingASkin]);
  for i:=Ord(Low(TSoundType)) to Ord(High(TSoundType)) do begin
      MainForm.Sound.SoundEnabled [TSoundType(i)]:=StrToBool(SettingsString[TSettings(3*i+Ord(stSoundMoveEnabled))]);
      MainForm.Sound.SoundFileName[TSoundType(i)]:=SettingsString[TSettings(3*i+Ord(stSoundMoveFileName))];
      end;
//MainForm.Sound.SoundFileTypeFilter:=ExternalToInternalFileTypeFilter(SettingsString[stSoundFileTypeFilter]);

{$IFDEF MUSIC_PLAYER}
  MainForm.Music.Enabled:=StrToBool(SettingsString[stMusicEnabled]);
  i:=StrIndexOfCI(FSettingsString[stMusicSource],MusicSourceText);
  if i>=0 then MainForm.Music.MusicSource:=TMusicSource(i);
  if SettingsString[stMusicCDDrive]<>'' then
     MainForm.Music.CDDrive:=AnsiUpperCase(SettingsString[stMusicCDDrive])[1];
  MainForm.Music.MusicFilePath:=SettingsString[stMusicFilePath];
  MainForm.Music.PlayListFileName:=SettingsString[stMusicPlayListFileName];
  MainForm.Music.MusicFileTypeFilter:=ExternalToInternalFileTypeFilter(SettingsString[stMusicFileTypeFilter]);

  with MainForm.MPlayer do begin
    Transparency:=StrToBool(SettingsString[stMusicPlayerTransparency]);
    TransparencyPct:=StrToInt(SettingsString[stMusicPlayerTransparencyPct]);
    i:=StrIndexOfCI(FSettingsString[stMusicPlayerColor],MusicPlayerColorText);
    if i>=0 then RGBShift:=TRGBShift(i);
    Display.TransparencyPct:=StrToInt(SettingsString[stMusicPlayerDisplayTransparencyPct]);
    SaveFont(MPlayerDisplayPanel.Font,stMusicPlayerDisplayFontName);
    if Display<>nil then begin
       Display.Font:=MPlayerDisplayPanel.Font;
       Display.ShowFps:=StrToBool(SettingsString[stMusicPlayerDisplayShowFps]);
       i:=StrIndexOfCI(FSettingsString[stMusicPlayerDisplayTitleMode],MusicPlayerTitleModeText);
       if i>=0 then Display.TitleMode:=TMPlayerTitleMode(i);
       i:=StrIndexOfCI(FSettingsString[stMusicPlayerDisplayActivitiesActivity],ActivityText);
       if i>=0 then Display.Activity:=TActivity(i);

       if Display.ActivitiesMenu<>nil then with Display do with ActivitiesMenu.MenuPanel.Font do begin
          ActivitiesMenu.TransparencyPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesMenuTransparencyPct]);
          ActivitiesMenu.TextShadow:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesMenuTextShadow]);
          if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesMenuColorsBackground]),True,i) then
             ActivitiesMenu.MenuPanel.Color:=TColor(i);
          if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesMenuColorsButtonText]),True,i) then
             ActivitiesMenu.ButtonFontColor[Menu_.bsEnabled]:=TColor(i);
          if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesMenuColorsFocusedButtonText]),True,i) then
             ActivitiesMenu.ButtonFontColor[Menu_.bsFocusedEnabled]:=TColor(i);
          if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesMenuColorsGrayedButtonText]),True,i) then
             ActivitiesMenu.ButtonFontColor[Menu_.bsDisabled]:=TColor(i);
          ActivitiesMenu.ButtonFontColor[Menu_.bsFocusedDisabled]:=ActivitiesMenu.ButtonFontColor[Menu_.bsDisabled];
          if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesMenuColorsTextShadow]),True,i) then
             with ActivitiesMenu do
               for b:=Low(TextShadowColor) to High(TextShadowColor) do
                   TextShadowColor[b]:=TColor(i);
          SaveFont(Display.ActivitiesMenu.MenuPanel.Font,stMusicPlayerDisplayActivitiesMenuFontName);
          end;
       end;
    end;

  with Mandala do begin
    with Engine do begin
      PaletteSpinSpeed.StepsPerSecondMin:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMin]);
      PaletteSpinSpeed.StepsPerSecondMax:=Max(PaletteSpinSpeed.StepsPerSecondMin,StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMax]));
      PaletteSpinSpeed.ChangeIntervalMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingInterval]);
      PaletteSpinSpeed.ChangeDeviationPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingDeviation]);
      PaletteChangeSpeed.StepsPerSecondMin:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMin]);
      PaletteChangeSpeed.StepsPerSecondMax:=Max(PaletteChangeSpeed.StepsPerSecondMin,StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMax]));
      PaletteChangeSpeed.ChangeIntervalMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeInterval]);
      PaletteChangeSpeed.ChangeDeviationPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeDeviation]);
      PaletteEffectsIntervalMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsInterval]);
      PaletteEffectsDeviationPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsDeviation]);
      PatternChangeIntervalMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternInterval]);
      PatternChangeDeviationPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternDeviation]);
      PatternChangeFadeSteps:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFadeSteps]);
      PatternChangeFadeStepIntervalMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternFadeStepsInterval]);
      PatternChangePixelMorphIntervalMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternMorphStepsInterval]);
      ShowTitleOnStartup:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineShowTitleOnStartup]);
      end;

    Menu.TransparencyPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuTransparencyPct]);
    Menu.TextShadow:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuTextShadow]);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsBackground]),True,i) then
           Menu.MenuPanel.Color:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsFocusedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsFocusedEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsGrayedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsDisabled]:=TColor(i);
    Menu.ButtonFontColor[Menu_.bsFocusedDisabled]:=Menu.ButtonFontColor[Menu_.bsDisabled];
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaMenuColorsTextShadow]),True,i) then
       with Menu do
         for b:=Low(TextShadowColor) to High(TextShadowColor) do
             TextShadowColor[b]:=TColor(i);
    SaveFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesMandalaMenuFontName);
    end;

  with Viewer1 do begin
    FileName:=SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerFileName];
    i:=StrIndexOfCI(FSettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerResizeToWindowWidth],Viewer1ResizeToWindowWidthText);
    if i>=0 then Data.ResizeToWindowWidth:=Boolean(i);
    Data.SlideShow:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowEnabled]);
    Data.TimeDelayMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeDelayMS]);
    Data.TimeFadeMS:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeFadeMS]);
    Data.RandomOrder:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowRandomOrder]);
    Menu.TransparencyPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuTransparencyPct]);
    Menu.TextShadow:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuTextShadow]);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsBackground]),True,i) then
           Menu.MenuPanel.Color:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsFocusedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsFocusedEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsGrayedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsDisabled]:=TColor(i);
    Menu.ButtonFontColor[Menu_.bsFocusedDisabled]:=Menu.ButtonFontColor[Menu_.bsDisabled];
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuColorsTextShadow]),True,i) then
       with Menu do
         for b:=Low(TextShadowColor) to High(TextShadowColor) do
             TextShadowColor[b]:=TColor(i);
    SaveFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesImageViewerMenuFontName);
    if PictThread<>nil then PictThread.FileName:=FileName; // updates the filelist
    end;
{
  with Fireworks do begin
    with BuiltinfireworkTemplate[ftRocket1Color].Timer do begin
      Interval:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsInterval]);
      IntervalDeviationPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsDeviation]);
      IntervalDeviation:=(Cardinal(IntervalDeviationPct)*Interval) div 100;
      Enabled:=Interval<>0;
      end;
    with BuiltinfireworkTemplate[ftRocket2Colors].Timer do begin
      Interval:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsInterval]);
      IntervalDeviationPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsDeviation]);
      IntervalDeviation:=(Cardinal(IntervalDeviationPct)*Interval) div 100;
      Enabled:=Interval<>0;
      end;
    Menu.TransparencyPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuTransparencyPct]);
    Menu.TextShadow:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuTextShadow]);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsBackground]),True,i) then
           Menu.MenuPanel.Color:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsFocusedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsFocusedEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsGrayedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsDisabled]:=TColor(i);
    Menu.ButtonFontColor[Menu_.bsFocusedDisabled]:=Menu.ButtonFontColor[Menu_.bsDisabled];
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFireworksMenuColorsTextShadow]),True,i) then
       with Menu do
         for b:=Low(TextShadowColor) to High(TextShadowColor) do
             TextShadowColor[b]:=TColor(i);
    SaveFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesFireworksMenuFontName);
    end;
}
  with Fractals do begin
    if PaletteFileName<>SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsPaletteFileName] then
       Finalize;
    PaletteFileName:=SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsPaletteFileName];
    AntiAliasing:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsAntiAliasing]);
    Menu.TransparencyPct:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuTransparencyPct]);
    Menu.TextShadow:=StrToBool(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuTextShadow]);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsBackground]),True,i) then
           Menu.MenuPanel.Color:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsFocusedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsFocusedEnabled]:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsGrayedButtonText]),True,i) then
           Menu.ButtonFontColor[Menu_.bsDisabled]:=TColor(i);
    Menu.ButtonFontColor[Menu_.bsFocusedDisabled]:=Menu.ButtonFontColor[Menu_.bsDisabled];
    if SafeStrToInt(RGB_BGR(SettingsString[stMusicPlayerDisplayActivitiesActivitiesFractalsMenuColorsTextShadow]),True,i) then
       with Menu do
         for b:=Low(TextShadowColor) to High(TextShadowColor) do
             TextShadowColor[b]:=TColor(i);

    SaveFont(Menu.MenuPanel.Font,stMusicPlayerDisplayActivitiesActivitiesFractalsMenuFontName);
    end;

  with MPlayerForm do with FormColors do begin
    for i:=Ord(stMusicPlayerOpenWindowColorsBackground) to
           Ord(stMusicPlayerOpenWindowColorsFocusedWindowText) do
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(i)]),True,j) then
           case TSettings(i) of
             stMusicPlayerOpenWindowColorsBackground            : BackgroundColor:=TColor(j);
             stMusicPlayerOpenWindowColorsBackgroundText        : BackgroundTextColor:=TColor(j);
             stMusicPlayerOpenWindowColorsHighlightedText       : HighlightedTextColor:=TColor(j);
             stMusicPlayerOpenWindowColorsButton                : ButtonColor:=TColor(j);
             stMusicPlayerOpenWindowColorsButtonText            : ButtonTextColor:=TColor(j);
             stMusicPlayerOpenWindowColorsFocusedButton         : FocusedButtonColor:=TColor(j);
             stMusicPlayerOpenWindowColorsFocusedButtonText     : FocusedButtonTextColor:=TColor(j);
             stMusicPlayerOpenWindowColorsGrayedButton          : GrayedButtonColor:=TColor(j);
             stMusicPlayerOpenWindowColorsGrayedButtonText      : GrayedButtonTextColor:=TColor(j);
             stMusicPlayerOpenWindowColorsWindow                : WindowColor:=TColor(j);
             stMusicPlayerOpenWindowColorsWindowText            : WindowTextColor:=TColor(j);
             stMusicPlayerOpenWindowColorsFocusedWindow         : FocusedWindowColor:=TColor(j);
             stMusicPlayerOpenWindowColorsFocusedWindowText     : FocusedWindowTextColor:=TColor(j);
           end; // case
    end;
{$ENDIF}

  with MainForm.GamePictures do begin
    with Pictures[ptScreenBackground] do begin
        Visible:=StrToBool(SettingsString[stGraphicsBackgroundVisible]);
        //FileName:=SettingsString[stGraphicsBackgroundImageFileName]; // after 'SourceRect'
        SourceRect.Left:=StrToInt(SettingsString[stGraphicsBackgroundImageSectionLeft]);
        SourceRect.Top:=StrToInt(SettingsString[stGraphicsBackgroundImageSectionTop]);
        SourceRect.Right:=SourceRect.Left+StrToInt(SettingsString[stGraphicsBackgroundImageSectionWidth]);
        SourceRect.Bottom:=SourceRect.Top+StrToInt(SettingsString[stGraphicsBackgroundImageSectionHeight]);
        i:=StrIndexOfCI(FSettingsString[stGraphicsBackgroundImageView],ImageViewText);
        if i>=0 then View:=Misc_.TImageView(i);
        i:=StrIndexOfCI(FSettingsString[stGraphicsBackgroundImageAntialiasing],AntiAliasingText);
        if i>=0 then AntiAliasing:=Misc_.TAntiAliasing(i);
        if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsBackgroundColor]),True,i) then
           Color:=TColor(i);
        FileName:=SettingsString[stGraphicsBackgroundImageFileName];
        end;

    for i:=Ord(ptBoardBackground) to Ord(ptReverseModeBackground) do with Pictures[GView_.TPictureType(i)] do begin
        Visible:=StrToBool(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+1)]);
        //FileName:=SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+3)]; // after 'SourceRect'
        SourceRect.Left:=StrToInt(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+5)]);
        SourceRect.Top:=StrToInt(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+6)]);
        SourceRect.Right:=SourceRect.Left+StrToInt(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+7)]);
        SourceRect.Bottom:=SourceRect.Top+StrToInt(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+8)]);
        j:=StrIndexOfCI(FSettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+9)],ImageViewText);
        if j>=0 then View:=Misc_.TImageView(j);
        if View=ivFloorTile then View:=ivTile;
        j:=StrIndexOfCI(FSettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+10)],AntiAliasingText);
        if j>=0 then AntiAliasing:=Misc_.TAntiAliasing(j);
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+11)]),True,j) then
           MaskBitMapColor:=BitMap_.ColorToRGB(TColor(j));
        MaskBitMapPct:=StrToInt(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+12)]);
        Masked:=StrToBool(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+13)]);
        Transparency[GView_.TPictureType(i)]:=StrToBool(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+14)]);
        TransparencyPct[GView_.TPictureType(i)]:=StrToInt(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+15)]);
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+16)]),True,j) then
           Color:=TColor(j);
        FileName:=SettingsString[TSettings(17*(i-Ord(ptBoardBackground))+Ord(stGraphicsBoardBackground)+3)];
        end;

    with ReverseModePlayerStartPosition do begin
         Visible  := StrToBool(    SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionVisible   ] );
         GridSize := StrToInt(     SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridSize  ] );
         if SafeStrToInt( RGB_BGR( SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridColor ] ), True, j ) then
            GridColor := BitMap_.ColorToRGB( TColor( j ) );
         if SafeStrToInt( RGB_BGR( SettingsString[ stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridShadowColor ] ), True, j ) then
            GridShadowColor := BitMap_.ColorToRGB( TColor( j ) );
         end;

    for p:=ptPlayer to ptBoxOnGoalAnimation do with Pictures[p] do begin
        k:=PictureSettingsOffset[p];

        FileName:=SettingsString[TSettings(k+2)];
        SourceRect.Left:=StrToInt(SettingsString[TSettings(k+4)]);
        SourceRect.Top:=StrToInt(SettingsString[TSettings(k+5)]);
        SourceRect.Right:=SourceRect.Left+StrToInt(SettingsString[TSettings(k+6)]);
        SourceRect.Bottom:=SourceRect.Top+StrToInt(SettingsString[TSettings(k+7)]);
        Masked:=StrToBool(SettingsString[TSettings(k+8)]);
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(k+9)]),True,j) then
           MaskBitMapColor:=BitMap_.ColorToRGB(TColor(j));
        MaskBitMapPct:=StrToInt(SettingsString[TSettings(k+10)]);
        MaskExteriorOnly:=StrToBool(SettingsString[TSettings(k+11)]);
        j:=StrIndexOfCI(FSettingsString[TSettings(k+12)],AntiAliasingText);
        if j>=0 then AntiAliasing:=Misc_.TAntiAliasing(j);
        if p<ptPlayerAnimation then
           if (p<>ptFloor) and (p<>ptGoal) then begin
              if  SafeStrToInt(RGB_BGR(SettingsString[TSettings(k+13)]),True,j) then Color:=TColor(j);
              end
           else begin
              if  SafeStrToInt(RGB_BGR(SettingsString[TSettings(k+15)]),True,j) then Color:=TColor(j);
              end;
        if      p=ptPlayerOnGoal then begin
                MainForm.GamePictures.UsePlayerImageForMoveAnimationAlsoForGoalSquares               :=StrToBool(SettingsString[TSettings(k+14)]);
                MainForm.GamePictures.UsePlayerImageForHighlightingReachableSquaresAlsoForGoalSquares:=StrToBool(SettingsString[TSettings(k+15)]);
                end
        else if p=ptBoxOnGoal then begin
                MainForm.GamePictures.UseBoxImageForMoveAnimationAlsoForGoalSquares               :=StrToBool(SettingsString[TSettings(k+14)]);
                MainForm.GamePictures.UseBoxImageForHighlightingReachableSquaresAlsoForGoalSquares:=StrToBool(SettingsString[TSettings(k+15)]);
                end
        else if p=ptWall then begin
                j:=StrIndexOfCI(FSettingsString[TSettings(k+14)],WallTypeText);
                if j< 0 then j:=StrIndexOfCI(FSettingsString[TSettings(k+14)],OldWallTypeText);
                if j>=0 then MainForm.GamePictures.WallType:=TWallType(j);
                if SafeStrToInt(SettingsString[TSettings(k+16)],False,j) then MainForm.GamePictures.WallCap.X:=j;
                if SafeStrToInt(SettingsString[TSettings(k+17)],False,j) then MainForm.GamePictures.WallCap.Y:=j;
                if SafeStrToInt(SettingsString[TSettings(k+19)],False,j) then MainForm.GamePictures.OuterWallTrimming.Left:=j;
                if SafeStrToInt(SettingsString[TSettings(k+20)],False,j) then MainForm.GamePictures.OuterWallTrimming.Top:=j;
                if SafeStrToInt(SettingsString[TSettings(k+21)],False,j) then MainForm.GamePictures.OuterWallTrimming.Right:=j;
                if SafeStrToInt(SettingsString[TSettings(k+22)],False,j) then MainForm.GamePictures.OuterWallTrimming.Bottom:=j;
                end
        else if (p=ptFloor) or (p=ptGoal) then begin
                MainForm.GamePictures.Transparency   [p]:=StrToBool(SettingsString[TSettings(k+13)]);
                if SafeStrToInt(SettingsString[TSettings(k+14)],False,j) then MainForm.GamePictures.TransparencyPct[p]:=j;
                if p=ptFloor then Visible:=StrToBool(SettingsString[TSettings(k+16)]);
                end;
        if (p=ptPlayerAnimation) or (p=ptPlayerOnGoalAnimation) or
           (p=ptBoxAnimation   ) or (p=ptBoxOnGoalAnimation   ) then begin
           j:=StrIndexOfCI(FSettingsString[TSettings(k+13)],InBetweenFramesTypeText);
           if j>=0 then MainForm.GamePictures.InBetweenFramesType[p]:=Misc_.TInBetweenFramesType(j);
           end;
        end;

      MainForm.GamePictures.MaxZoomFactorPct:=StrToInt(SettingsString[stGraphicsBoardFiguresMaxZoomFactorPct]);
      MainForm.GamePictures.GridEnabled:=StrToBool(SettingsString[stGraphicsBoardGridEnabled]);
      if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsBoardGridColor1]),True,j) then
         MainForm.GamePictures.GridColor1:=TColor(j);
      if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsBoardGridColor2]),True,j) then
         MainForm.GamePictures.GridColor2:=TColor(j);
    end;

  with MainForm.Menu do begin
    Transparency:=StrToBool(SettingsString[stGraphicsMenuTransparentButtons]);
    TextShadow:=StrToBool(SettingsString[stGraphicsMenuTextShadow]);
    for i:=Ord(Low(ButtonPicture)) to Ord(High(ButtonPicture)) do begin
        ButtonPicture[TButtonState(i)].FileName:=SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 2)];
        TileRect[TButtonState(i)].Left:=StrToInt(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 4)]);
        TileRect[TButtonState(i)].Top:=StrToInt(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 5)]);
        TileRect[TButtonState(i)].Right:=TileRect[TButtonState(i)].Left+StrToInt(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 6)]);
        TileRect[TButtonState(i)].Bottom:=TileRect[TButtonState(i)].Top+StrToInt(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 7)]);
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 8)]),True,j) then
           ButtonPicture[TButtonState(i)].MaskBitMapColor:=BitMap_.ColorToRGB(TColor(j));
        ButtonPicture[TButtonState(i)].MaskBitMapPct:=StrToInt(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+ 9)]);
        ButtonPicture[TButtonState(i)].Masked:=StrToBool(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+10)]);
        TransparencyPct[TButtonState(i)]:=StrToInt(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+11)]);
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+12)]),True,j) then
           ButtonFontColor[TButtonState(i)]:=TColor(j);
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(14*i+Ord(stGraphicsMenuDisabledButton)+13)]),True,j) then
           TextShadowColor[TButtonState(i)]:=TColor(j);
        end;
    EdgeSmoothing:=StrToBool(SettingsString[stGraphicsMenuButtonsEdgesEdgeSmoothing]);
    EdgeTransparencyPct:=StrToInt(SettingsString[stGraphicsMenuButtonsEdgesTransparencyPct]);
    SaveFont(MainForm.MenuPanel.Font,stGraphicsMenuButtonsFontName);
    end;

  with MainForm.RotateAndflipPopupMenu do with FormColors do begin
    for i:=Ord(stGraphicsMenuPopupMenuColorsBackground) to
           Ord(stGraphicsMenuPopupMenuColorsFocusedWindowText) do
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(i)]),True,j) then
           case TSettings(i) of
             stGraphicsMenuPopupMenuColorsBackground            : BackgroundColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsBackgroundText        : BackgroundTextColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsHighlightedText       : HighlightedTextColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsButton                : ButtonColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsButtonText            : ButtonTextColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsFocusedButton         : FocusedButtonColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsFocusedButtonText     : FocusedButtonTextColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsGrayedButton          : GrayedButtonColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsGrayedButtonText      : GrayedButtonTextColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsWindow                : WindowColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsWindowText            : WindowTextColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsFocusedWindow         : FocusedWindowColor:=TColor(j);
             stGraphicsMenuPopupMenuColorsFocusedWindowText     : FocusedWindowTextColor:=TColor(j);
           end; // case
    MainForm.MultiViewPopupMenu.FormColors:=MainForm.RotateAndflipPopupMenu.FormColors;
    end;

  with MainForm.Status do begin
    FileName:=SettingsString[stGraphicsStatusImageFileName];
    TileRect.Left:=StrToInt(SettingsString[stGraphicsStatusImageSectionLeft]);
    TileRect.Top:=StrToInt(SettingsString[stGraphicsStatusImageSectionTop]);
    TileRect.Right:=TileRect.Left+StrToInt(SettingsString[stGraphicsStatusImageSectionWidth]);
    TileRect.Bottom:=TileRect.Top+StrToInt(SettingsString[stGraphicsStatusImageSectionHeight]);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsStatusImageSectionBackgroundColor]),True,i) then
       MaskBitMapColor:=BitMap_.ColorToRGB(TColor(i));
    MaskBitMapPct:=StrToInt(SettingsString[stGraphicsStatusImageSectionBackgroundColorTolerancePct]);
    Masked:=StrToBool(SettingsString[stGraphicsStatusMasked]);
    TransparencyPct:=StrToInt(SettingsString[stGraphicsStatusTransparencyPct]);
    EdgeSmoothing:=StrToBool(SettingsString[stGraphicsStatusEdgesEdgeSmoothing]);
    EdgeTransparencyPct:=StrToInt(SettingsString[stGraphicsStatusEdgesTransparencyPct]);
    LeftSlice:=StrToInt(SettingsString[stGraphicsStatusImageSlicesLeft]);
    BodySlice:=StrToInt(SettingsString[stGraphicsStatusImageSlicesBody]);
    RightSlice:=StrToInt(SettingsString[stGraphicsStatusImageSlicesRight]);
    ButtonTransparencyPct:=StrToInt(SettingsString[stGraphicsStatusButtonTransparencyPct]);
    PanelBoundsTop:=StrToInt(SettingsString[stGraphicsStatusPanelsBoundsTop]);
    PanelBoundsBottom:=StrToInt(SettingsString[stGraphicsStatusPanelsBoundsBottom]);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsStatusPanelsColor]),True,i) then
       PanelColor:=TColor(i);
    PanelTransparencyPct:=StrToInt(SettingsString[stGraphicsStatusPanelsTransparencyPct]);
    PanelRoundRect:=StrToInt(SettingsString[stGraphicsStatusPanelsRoundingRectanglePixels]);
    SaveFont(MainForm.StatusPanel.Font,stGraphicsStatusFontName);
    SubTextsAreTransparent:=StrToBool(SettingsString[stGraphicsStatusSubTextsAreTransparent]);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsStatusSubTextsFontColor]),True,i) then
       SubTextsFontColor:=TColor(i);
    SubTextsFontSize:=StrToInt(SettingsString[stGraphicsStatusSubTextsFontSize]);
    TextShadow:=StrToBool(SettingsString[stGraphicsStatusTextShadow]);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsStatusTextShadowColor]),True,i) then
         TextShadowColor:=TColor(i);
    end;

  with MainForm.MultiView do begin
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsMultipleViewsColorsBackground]),True,i) then
       BackgroundColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsMultipleViewsColorsFocusedBackground]),True,i) then
       FocusedBackgroundColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsMultipleViewsColorsLine]),True,i) then
       LineColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsMultipleViewsColorsText]),True,i) then
       TextColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsMultipleViewsColorsFocusedText]),True,i) then
       FocusedTextColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsMultipleViewsColorsShadow]),True,i) then
       ShadowColor:=TColor(i);
    BackgroundTransparencyPct:=StrToInt(SettingsString[stGraphicsMultipleViewsBackgroundTransparencyPct]);
    SaveFont(Font,stGraphicsMultipleViewsFontName);
    end;

  with ToolsForm do begin
    SaveFont(Font,stGraphicsToolsWindowFontName);
    ToolsForm.OnFontChange;
    ButtonsFileName:=SettingsString[stGraphicsToolsButtonsImageFileName];
    if StrEqual(ButtonsFileName,DEFAULT_VALUE) then ButtonsFileName:='';

    with GameViewer.BackgroundPict do begin
      Visible:=StrToBool(SettingsString[stGraphicsToolsBoardBackgroundVisible]);
      FileName:=SettingsString[stGraphicsToolsBoardBackgroundImageFileName];
      if StrEqual(FileName,DEFAULT_VALUE) then FileName:='';
      i:=StrIndexOfCI(FSettingsString[stGraphicsToolsBoardBackgroundImageView],ImageViewText);
      if i>=0 then View:=Misc_.TImageView(i);
      if View=ivFloorTile then View:=ivTile;
      i:=StrIndexOfCI(FSettingsString[stGraphicsToolsBoardBackgroundImageAntialiasing],AntiAliasingText);
      if i>=0 then AntiAliasing:=Misc_.TAntiAliasing(i);
      if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsBoardBackgroundColor]),True,i) then
         Color:=TColor(i);
      end;

    with GameViewer.SkinPict do begin
      FileName:=SettingsString[stGraphicsToolsBoardSkinImageFileName];
      if StrEqual(FileName,DEFAULT_VALUE) then FileName:='';
      Masked:=StrToBool(SettingsString[stGraphicsToolsBoardSkinImageMasked]);
      if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsBoardSkinImageBackgroundColor]),True,j) then
         MaskBitMapColor:=BitMap_.ColorToRGB(TColor(j));
      MaskBitMapPct:=StrToInt(SettingsString[stGraphicsToolsBoardSkinImageBackgroundColorTolerancePct]);
      j:=StrIndexOfCI(FSettingsString[stGraphicsToolsBoardSkinImageAntialiasing],AntiAliasingText);
      if j>=0 then AntiAliasing:=Misc_.TAntiAliasing(j);
      end;

    with GameViewer do begin
      if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsBoardSquareSetSelectedSquaresColor]),True,j) then
         SquareSetSelectedSquaresColor:=TColor(j);
      if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsBoardSquareSetNotSelectedSquaresColor]),True,j) then
         SquareSetNotSelectedSquaresColor:=TColor(j);
      SquareSetTransparencyPct:=StrToInt(SettingsString[stGraphicsToolsBoardSquareSetTransparencyPct]);
      end;

    for i:=Ord(Low(Tools_.TEditorCursorType)) to Ord(High(Tools_.TEditorCursorType)) do with Editor.Cursors[Tools_.TEditorCursorType(i)] do begin
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(i*4+Ord(stGraphicsToolsEditorCursorsCellCursor)+1)]),True,j) then
           PenColor:=TColor(j);
        PenWidth:=Max(1,Min(Tools_.MAX_CURSOR_PEN_WIDTH,StrToInt(SettingsString[TSettings(i*4+Ord(stGraphicsToolsEditorCursorsCellCursor)+2)])));
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(i*4+Ord(stGraphicsToolsEditorCursorsCellCursor)+3)]),True,j) then
           ShadowColor:=TColor(j);
        end;

    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsEditorFrameColor]),True,i) then
       GameViewer.FrameColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsEditorFrameShadowColor]),True,i) then
       GameViewer.FrameShadowColor:=TColor(i);
    ToolsForm.EditMenuItemGrid.Checked:=StrToBool(SettingsString[stGraphicsToolsEditorGridEnabled]);

    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsBackgroundColor]),True,j) then
       ReplaySpeedTrackBarBackgroundColor:=TColor(j);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsSliderColor]),True,j) then
       ReplaySpeedTrackBarSliderColor:=TColor(j);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsFontColor]),True,j) then
       ReplaySpeedTrackBarFontColor:=TColor(j);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsReplaySpeedTrackBarColorsshadowColor]),True,j) then
       ReplaySpeedTrackBarShadowColor:=TColor(j);

    ToolsForm.CollectionNameInTitleLine:=StrToBool(SettingsString[stWindowsToolsCollectionNameInTitleLine]);
    end;

  with CaptureForm do begin
    ToolsForm.CaptureSettingsMenuItemSnapToNearbyEdges.Checked := StrToBool( SettingsString[stToolsCaptureSnapToNearbyEdges] );
    if SafeStrToInt(RGB_BGR( SettingsString[stToolsCaptureGridColor] ), True, j ) then
       CaptureForm.Editor.GridColor := TColor( j );
    if SafeStrToInt(RGB_BGR( SettingsString[stToolsCaptureGridShadowColor] ), True, j ) then
       CaptureForm.Editor.GridShadowColor := TColor( j );
    CaptureForm.DefaultSkinFileNameText:=SettingsString[stToolsCaptureTextsDefaultSkinFileName];
    CaptureForm.Settings.CheckForDuplicatesWhenAppendingNewLevelsToCollections := StrToBool( SettingsString[stToolsCaptureAppendNewLevelsToCollectionsCheckForDuplicates] );
    end;

  with LevelSetForm do with FormColors do begin
    SaveFont(Font,stGraphicsToolsInternalClipboardWindowFontName);
    LevelSetForm.OnFontChange;
    for i:=Ord(stGraphicsToolsInternalClipboardColorsBackground) to
           Ord(stGraphicsToolsInternalClipboardColorsNotesWindow) do
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(i)]),True,j) then
           case TSettings(i) of
             stGraphicsToolsInternalClipboardColorsBackground            : BackgroundColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsBackgroundText        : BackgroundTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsHighlightedText       : HighlightedTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsButton                : ButtonColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsButtonText            : ButtonTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsFocusedButton         : FocusedButtonColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsFocusedButtonText     : FocusedButtonTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsGrayedButton          : GrayedButtonColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsGrayedButtonText      : GrayedButtonTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsWindow                : WindowColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsWindowText            : WindowTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsFocusedWindow         : FocusedWindowColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsFocusedWindowText     : FocusedWindowTextColor:=TColor(j);
             stGraphicsToolsInternalClipboardColorsNotesWindow           : InfoMemo.Color:=TColor(j);
           end; // case
    StandardClipboardItemName:=SettingsString[stGraphicsToolsInternalClipboardTextsClipboardItemName];
    SaveFont(InfoMemo.Font,stGraphicsToolsInternalClipboardEditFontName);
    end;

  with DuplicatesForm do begin
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsDuplicateLevelsColorsBackgroundColor1]),True,i) then
       BackgroundColor1:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsDuplicateLevelsColorsTextColor1]),True,i) then
       TextColor1:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsDuplicateLevelsColorsBackgroundColor2]),True,i) then
       BackgroundColor2:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsDuplicateLevelsColorsTextColor2]),True,i) then
       TextColor2:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsDuplicateLevelsColorsHighlightBackgroundColor]),True,i) then
       HighlightBackgroundColor:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsToolsDuplicateLevelsColorsHighlightTextColor]),True,i) then
       HighlightTextColor:=TColor(i);
    end;

  with SnapshotsForm do with FormColors do begin
    SaveFont(Font,stGraphicsSnapshotsWindowFontName);
    SnapshotsForm.OnFontChange;
    for i:=Ord(stGraphicsSnapshotsColorsBackground) to
           Ord(stGraphicsSnapshotsColorsNotesWindow) do
        if SafeStrToInt(RGB_BGR(SettingsString[TSettings(i)]),True,j) then
           case TSettings(i) of
             stGraphicsSnapshotsColorsBackground            : BackgroundColor:=TColor(j);
             stGraphicsSnapshotsColorsBackgroundText        : BackgroundTextColor:=TColor(j);
             stGraphicsSnapshotsColorsHighlightedText       : HighlightedTextColor:=TColor(j);
             stGraphicsSnapshotsColorsButton                : ButtonColor:=TColor(j);
             stGraphicsSnapshotsColorsButtonText            : ButtonTextColor:=TColor(j);
             stGraphicsSnapshotsColorsFocusedButton         : FocusedButtonColor:=TColor(j);
             stGraphicsSnapshotsColorsFocusedButtonText     : FocusedButtonTextColor:=TColor(j);
             stGraphicsSnapshotsColorsGrayedButton          : GrayedButtonColor:=TColor(j);
             stGraphicsSnapshotsColorsGrayedButtonText      : GrayedButtonTextColor:=TColor(j);
             stGraphicsSnapshotsColorsWindow                : WindowColor:=TColor(j);
             stGraphicsSnapshotsColorsWindowText            : WindowTextColor:=TColor(j);
             stGraphicsSnapshotsColorsFocusedWindow         : FocusedWindowColor:=TColor(j);
             stGraphicsSnapshotsColorsFocusedWindowText     : FocusedWindowTextColor:=TColor(j);
             stGraphicsSnapshotsColorsNotesWindow           : InfoMemo.Color:=TColor(j);
           end; // case
    SolutionName:=SettingsString[stGraphicsSnapshotsTextsSolution];
    BestSolutionName:=SettingsString[stGraphicsSnapshotsTextsBestSolution];
    BestSolutionMovesName:=SettingsString[stGraphicsSnapshotsTextsBestSolutionMoves];
    BestSolutionPushesName:=SettingsString[stGraphicsSnapshotsTextsBestSolutionPushes];
    NormalModeSnapshotName:=SettingsString[stGraphicsSnapshotsTextsNormalModeSnapshotName];
    ReverseModeSnapshotName:=SettingsString[stGraphicsSnapshotsTextsReverseModeSnapshotName];
    SaveFont(InfoMemo.Font,stGraphicsSnapshotsEditFontName);
    end;

  with OpenForm do begin
    SaveFont(Font,stGraphicsOpenWindowFontName);
    OpenForm.OnFontChange;
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsOpenWindowColorsCollectionBackgroundColor]),True,i) then
       CollectionStringGrid.Color:=TColor(i);
    if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsOpenWindowColorsCollectionTextColor]),True,i) then
       CollectionStringGrid.Font.Color:=TColor(i);
//  if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsOpenWindowColorsCollectionHighlightBackgroundColor]),True,i) then
//     CollectionHighlightBackgroundColor:=TColor(i);
//  if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsOpenWindowColorsCollectionHighlightTextColor]),True,i) then
//     CollectionHighlightTextColor:=TColor(i);
    end;

  with OptionsForm do begin
    SaveFont(Font,stGraphicsOptionsWindowFontName);
    OptionsForm.OnFontChange;
    end;

  with DuplicatesForm do begin
    SaveFont(Font,stGraphicsDuplicatesWindowFontName);
    DuplicatesForm.OnFontChange;
    end;

  with HelpForm do begin
    SaveFont(Font,stGraphicsHelpWindowFontName);
    HelpForm.OnFontChange;
    end;

//if EditForm<>nil then with EditForm do begin
//   if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsEditorColorsWall]),True,i) then
//      MainForm.Game.Color[WallColor]:=TColor(i);
//   if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsEditorColorsFloor]),True,i) then
//      MainForm.Game.Color[FloorColor]:=TColor(i);
//   if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsEditorColorsBox]),True,i) then
//      MainForm.Game.Color[BoxColor]:=TColor(i);
//   if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsEditorColorsGoal]),True,i) then
//      MainForm.Game.Color[GoalColor]:=TColor(i);
//   if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsEditorColorsPlayer]),True,i) then
//      MainForm.Game.Color[PlayerColor]:=TColor(i);
//   if SafeStrToInt(RGB_BGR(SettingsString[stGraphicsEditorColorsCursor]),True,i) then
//      CursorColor:=TColor(i);
//   InvalidateDrawingToolGlyphs;
//   end;

  if Modified then begin
     Modified:=False;
     if MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified <> '' then
        if   MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified[ 1 ] =  FILE_NAME_PATH_DELIMITER then // 'True': this is a new most recently loaded skin name
             MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified      := Copy( MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified, 2, MaxInt )
        else MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified      := '';
     end;
end;

procedure TOptionsForm.StringGrid1TopLeftChanged(Sender: TObject);
begin
  if not IsBusy then begin
     CloseEditors(True);
     if TreeView1.TopItem.AbsoluteIndex<>Ord(RowToSettings(StringGrid1.TopRow)) then begin
        TreeView1.TopItem:=TreeView1.Items[Ord(RowToSettings(StringGrid1.TopRow))];
        end;
{
     with TreeView1 do
       if (Selected<>nil) then
          if      Selected.AbsoluteIndex< TopItem.AbsoluteIndex then Selected:=TopItem
//        doesn't work: (reason unknown)
//        else if Selected.AbsoluteIndex>=TopItem.AbsoluteIndex+StringGrid1.VisibleRowCount then
//                Selected:=Items[TopItem.AbsoluteIndex+Pred(StringGrid1.VisibleRowCount)]
          ;
}
     end;
end;


procedure TOptionsForm.StringGrid1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin {
  if        Key=VK_RETURN then
            if   TreeView1.Selected.HasChildren then begin
                 TreeView1.Selected.Expanded:=not TreeView1.Selected.Expanded;
                 TreeToGrid;
                 end
            else if StringGrid1.Col=0 then
                    begin ShowEditor(StringGrid1.Row);
                          if SelectBtn.Visible and
                             //Panel1.Visible and
                             (SettingsEditor[RowToSettings(EditorRow)]<>seColor) then
                             Panel1Click(Sender);
                    end
                 else
  else}if   (Key=VK_LEFT) or (Key=VK_RIGHT) then
            TreeView1.SetFocus
  else;
end;

procedure TOptionsForm.StringGrid1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if        Key=VK_RETURN then
            if   TreeView1.Selected.HasChildren then begin
                 TreeView1.Selected.Expanded:=not TreeView1.Selected.Expanded;
                 TreeToGrid;
                 end
            else if StringGrid1.Col=0 then
                    begin ShowEditor(StringGrid1.Row);
                          if SelectBtn.Visible and
                             //Panel1.Visible and
                             (SettingsEditor[RowToSettings(EditorRow)]<>seColor) then
                             Panel1Click(Sender);
                    end
                 else
{ else if   (Key=VK_LEFT) or (Key=VK_RIGHT) then
            TreeView1.SetFocus}
  else;
end;

procedure TOptionsForm.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect:=(ACol<StringGrid1.ColCount) and (ARow<StringGrid1.RowCount);
  if CanSelect then begin
     //StatusBar1.Panels[0].Text:=IntToStr(Ord(RowToSettings(ARow)))+SPACE+StringGrid1.Cells[0,ARow];
     if (TreeView1.Selected.Data<>StringGrid1.Objects[0,ARow]) and
        StringGrid1.Focused then begin
        TreeView1.Selected:=TreeView1.Items[Ord(RowToSettings(ARow))];
        end;
     end;
end;

procedure TOptionsForm.StringGrid1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var ACol,ARow:LongInt; CanSelect:Boolean;
begin
  if (Button=mbLeft) then
     if   StringGrid1.Cursor<>crDefault then
          begin IsResizing:=True; DragPoint:=Point(X,Y); HideEditors;
          end
     else begin StringGrid1.MouseToCell(X,Y,ACol,ARow);
                if (ACol>=StringGrid1.FixedCols) and (ACol<StringGrid1.ColCount) and
                   (ARow>=StringGrid1.FixedRows) and (ACol<StringGrid1.RowCount) then begin
                   StringGrid1SelectCell(Sender,ACol,ARow,CanSelect);
                   if CanSelect then with StringGrid1 do
                      begin Col:=ACol; Row:=ARow; ShowEditor(Row);
                      end;
                   end;
          end;
end;

procedure TOptionsForm.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if IsResizing then
     begin IsResizing:=False; StringGrid1.Cursor:=crDefault;
     end
  else if (Button=mbRight) and (not Modified) then
          CancelBtnClick(Sender);
end;

procedure TOptionsForm.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var NewLeft,RightBound:Integer;
begin
  if IsResizing then with StringGrid1 do
     begin RightBound:=TreeView1.Left+TreeView1.Width-2-GetSystemMetrics(SM_CXVSCROLL);
//         NewLeft:=Min(Max(TreeView1.Left+2*Panel1.Width,Left+X-DragPoint.X),RightBound-2*Panel1.Width);
           NewLeft:=Min(Max(TreeView1.Left+2*SelectBtn.Width,Left+X-DragPoint.X),RightBound-2*SelectBtn.Width);
           Width:=RightBound-NewLeft;
           Left:=NewLeft;
           ColWidths[0]:=ClientWidth-GridLineWidth;
     end
  else if      X<=6 then
               StringGrid1.Cursor:=crHSplit
       else if StringGrid1.Cursor<>crDefault then StringGrid1.Cursor:=crDefault;
end;

function  TOptionsForm.GetSettingsHint(No:TSettings):String;
begin
  if   (No<stGraphicsBoardFiguresWallCap) or
       (No>stGraphicsBoardFiguresWallCapTop) then
       Result:=TreeView1.Items[Ord(No)].Text
  else Result:=TreeView1.Items[Ord(No)].Text+HintNegativeValueMeansCenteredWallCapText;
end;

procedure TOptionsForm.ShowEditor(Row:Integer);
var i:Integer; Setting:TSettings;

  procedure ShowControl(const Control__:TWinControl; Activate__:Boolean);
  begin
    HideEditors;
    if (Control__<>StringGrid1) and (not IsResizing) then with Control__ do
       begin Left :=StringGrid1.Left+StringGrid1.GridLineWidth*2+0;
             Top  :=StringGrid1.Top +(EditorRow-StringGrid1.TopRow)*(StringGrid1.DefaultRowHeight+StringGrid1.GridLineWidth);
             if   Control__ is TPanel then with Control__ as TPanel do
                  begin Edit1.Text:=StringGrid1.Cells[0,EditorRow];
                        ShowControl(Edit1,True); Activate__:=False;
                        Top:=Top-1;
//                      Height:=StringGrid1.RowHeights[EditorRow]+2;
                        Height:=Edit1.Height+1;
                        Width :=Height;
                        Left  :=Left+StringGrid1.ColWidths[0]-TPanel(Control__).Width-1;
                        Edit1.Width:=Edit1.Width-TPanel(Control__).Width;
                        BringToFront;
                  end
             else if Control__ is TButton then with Control__ as TButton do
                  begin Edit1.Text:=StringGrid1.Cells[0,EditorRow];
                        ShowControl(Edit1,True); Activate__:=False;
                        Top:=Top-1;
                        Height:=Edit1.Height+1;
                        Width :=Height;
                        Left  :=Left+StringGrid1.ColWidths[0]-TPanel(Control__).Width-1;
                        Edit1.Width:=Edit1.Width-TButton(Control__).Width;
                        BringToFront;
                  end
             else begin if Control__=Edit1 then with Edit1 do begin
                           //ReadOnly:=RowToSettings(EditorRow)=stSoundFileTypeFilter;
                           SetEditColor;
                           if   SettingsEditor[RowToSettings(EditorRow)]=seColor then
                                CharCase:=ecUpperCase
                           else CharCase:=ecNormal;
                           end;
                        Width:=StringGrid1.ColWidths[0]+StringGrid1.GridLineWidth-1;
                        Top:=Top+Max(0,StringGrid1.RowHeights[EditorRow]+StringGrid1.GridLineWidth*2-Height);

                  end;
             Show;
             if Activate__ then ActiveControl:=Control__;
       end;
  end;

  procedure FillComboBox(Min,Max:Integer; const Texts:array of string);
  var i:Integer;
  begin
    with ComboBox1 do
      begin LoadComboBoxFromStrings(Min,Max,Texts,{ComboBox1.Canvas}Self.Canvas {kludge: for some unknown reason, it's not possible to use 'ComboBox.Canvas' here},ComboBox1);
            for i:=Min to Max do
                if StringGrid1.Cells[0,EditorRow]=Texts[i] then ItemIndex:=i;
            ShowControl(ComboBox1,True);
      end;
  end;

begin {ShowEditor}
  EditorRow:=Row; Setting:=RowToSettings(EditorRow); HideEditors;
  GridScrollInView(StringGrid1,Row);

  case SettingsEditor[Setting] of
    seNone         : ActiveControl:=StringGrid1;
    seBoolean      : FillComboBox(Ord(Low(Boolean)),Ord(High(Boolean)),BooleanText);
    seInteger,
    seText,
    seFileTypeFilter
                   : with Edit1 do
                       begin SelectBtn.Hide;
                             //Panel1.Hide; // in order to change edit1.text
                             Edit1.Text:=StringGrid1.Cells[0,EditorRow];
                             ShowControl(Edit1,True);
                       end;
    sePct          : with SpinEdit1 do
                       begin MinValue:=0; MaxValue:=100; Increment:=1;

                             {$IFDEF MUSIC_PLAYER}
                                case Setting of
                                     stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMin : MaxValue:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMax]);
                                     stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMax : MinValue:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingSpeedMin]);
                                     stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMin  : MaxValue:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMax ]);
                                     stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMax  : MinValue:=StrToInt(SettingsString[stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeSpeedMin ]);
                                end; // case
                             {$ENDIF}

                             if   SafeStrToInt(StringGrid1.Cells[0,EditorRow],False,i) then
                                  Value:=i
                             else begin Value:=MinValue;
                                        StringGrid1.Cells[0,EditorRow]:=IntToStr(Value);
                                  end;
                             ShowControl(SpinEdit1,True);
                       end;
    seSpin         : with SpinEdit1 do
                       begin MinValue:=-MaxInt; MaxValue:=MaxInt; Increment:=1; // ensure value is not clipped
                             if   SafeStrToInt(StringGrid1.Cells[0,EditorRow],False,i) then
                                  Value:=i
                             else begin Value:=MinValue;
                                        StringGrid1.Cells[0,EditorRow]:=IntToStr(Value);
                                  end;
//                           if Setting=stSmoothMoveAnimationThresholdMaxPixelsPerMove then
//                              begin MinValue:=1; MaxValue:=MAX_SMOOTH_ANIMATION_THRESHOLD_MAX_PIXELS_PER_MOVE;
//                              end
//                           else
                             if Setting in [stMoveAnimationDoMove,
                                            stMoveAnimationUndoMove,
                                            stMoveAnimationReplayMoves] then
                                begin MinValue:=0; MaxValue:=1000;
                                end
//                           else if  (Setting>=stSolutionsScoreMetricsWeightsBoxLines) and
//                                    (Setting<=stSolutionsScoreMetricsWeightsPushingSessions) then
//                              begin MinValue:=0; MaxValue:=MAX_SCORE_METRICS_WEIGHT;
//                              end
                             else if  Setting=stPathFindingShowLegalMovesBoxCursorPenWidth then
                                begin MinValue:=1; MaxValue:=MAX_LEGAL_MOVES_CURSOR_PEN_WIDTH;
                                end
                             else if  Setting=stPathFindingMaxTime then
                                begin MinValue:=MIN_PATH_FINDING_MAX_TIME_MS; MaxValue:=MAX_PATH_FINDING_MAX_TIME_MS;
                                      Increment:=1000;
                                end
                             else if  Setting=stPathFindingPushesLowerBoundCalculationMaxTime then
                                begin MinValue:=MIN_PATH_FINDING_MAX_TIME_MS; MaxValue:=MAX_PUSHES_LOWER_BOUND_CALCULATION_MAX_TIME_MS;
                                      Increment:=1000;
                                end
                             else if  Setting=stTimingIdleTimeThresholdSeconds then
                                begin MinValue:=0; MaxValue:=MAX_TIMING_IDLE_TIME_Threshold_MS  div 1000;
                                end
                             else if  Setting=stWindowsMainLeft then
                                begin MinValue:=Screen.DeskTopLeft; MaxValue:=Max(MinValue,Screen.DeskTopLeft+Screen.DeskTopWidth -MIN_MAIN_WINDOW_WIDTH);
                                end
                             else if  Setting=stWindowsMainTop then
                                begin MinValue:=Screen.DeskTopTop;  MaxValue:=Max(MinValue,Screen.DeskTopTop +Screen.DeskTopHeight-MIN_MAIN_WINDOW_HEIGHT-40);
                                end
                             else if  Setting=stWindowsMainWidth then
                                begin MinValue:=MIN_MAIN_WINDOW_WIDTH;  MaxValue:=Max(MinValue,Screen.DeskTopWidth);
                                end
                             else if  Setting=stWindowsMainHeight then
                                begin MinValue:=MIN_MAIN_WINDOW_HEIGHT; MaxValue:=Max(MinValue,Screen.DeskTopHeight);
                                end
                             else if  Setting=stWindowsOpenLeft then
                                begin MinValue:=Screen.DeskTopLeft; MaxValue:=Max(MinValue,Screen.DeskTopLeft+Screen.DeskTopWidth -OpenForm.CalculateMinWidth);
                                end
                             else if  Setting=stWindowsOpenTop then
                                begin MinValue:=Screen.DeskTopTop;  MaxValue:=Max(MinValue,Screen.DeskTopTop +Screen.DeskTopHeight-OpenForm.CalculateMinHeight-40);
                                end
                             else if  Setting=stWindowsOpenWidth then
                                begin MinValue:=OpenForm.CalculateMinWidth;  MaxValue:=Max(MinValue,Screen.DeskTopWidth);
                                end
                             else if  Setting=stWindowsOpenHeight then
                                begin MinValue:=OpenForm.CalculateMinHeight; MaxValue:=Max(MinValue,Screen.DeskTopHeight);
                                end
                             else if  Setting=stWindowsOpenDiskGroupBoxWidth then
                                begin MinValue:=OpenForm.DiskGroupBoxMinimumWidth;  MaxValue:=Max(MinValue,Screen.DeskTopWidth);
                                end
                             else if  Setting=stWindowsOpenDiskGroupBoxHeight then
                                begin MinValue:=OpenForm.DiskGroupBoxMinimumHeight; MaxValue:=Max(MinValue,Screen.DeskTopHeight);
                                end
                             else if  Setting=stWindowsOpenFolderListBoxWidth then
                                begin MinValue:=OpenForm.FolderListBoxMinimumWidth;  MaxValue:=Max(MinValue,Screen.DeskTopWidth);
                                end
                             else if  Setting=stWindowsOpenLevelPreviewHeight then
                                begin MinValue:=OpenForm.DiskGroupBoxMinimumHeight-OpenForm.GameBoardPanel.Top; MaxValue:=Max(MinValue,Screen.DeskTopHeight);
                                end
                             else if  Setting=stWindowsToolsLeft then
                                begin MinValue:=Screen.DeskTopLeft;  MaxValue:=Max(MinValue,Screen.DeskTopLeft+Screen.DeskTopWidth -ToolsForm.MinWidth);
                                end
                             else if  Setting=stWindowsToolsTop then
                                begin MinValue:=Screen.DeskTopTop;   MaxValue:=Max(MinValue,Screen.DeskTopTop +Screen.DeskTopHeight-ToolsForm.MinHeight-40);
                                end
                             else if  Setting=stWindowsToolsWidth then
                                begin MinValue:=ToolsForm.MinWidth;  MaxValue:=Max(MinValue,Screen.DeskTopWidth);
                                end
                             else if  Setting=stWindowsToolsHeight then
                                begin MinValue:=ToolsForm.MinHeight; MaxValue:=Max(MinValue,Screen.DeskTopHeight);
                                end
                             else if  (Setting=stWindowsOpenToolTipsOffsetX) or
                                      (Setting=stWindowsOpenToolTipsOffsetY) then
                                begin MinValue:=-MAX_TOOL_TIPS_OFFSET_PIXELS; MaxValue:=-MinValue;
                                end
                             else if  (Setting=stWindowsSnapshotsLeft) or (Setting=stWindowsInternalClipboardLeft) then
                                begin MinValue:=Screen.DeskTopLeft; MaxValue:=Max(MinValue,Screen.DeskTopLeft+Screen.DeskTopWidth -SnapshotsForm.MinimumClientWidth);
                                end
                             else if  (Setting=stWindowsSnapshotsTop) or (Setting=stWindowsInternalClipboardTop)  then
                                begin MinValue:=Screen.DeskTopTop;  MaxValue:=Max(MinValue,Screen.DeskTopTop +Screen.DeskTopHeight-SnapshotsForm.MinimumClientHeight-40);
                                end
                             else if  (Setting=stWindowsSnapshotsWidth) or (Setting=stWindowsInternalClipboardWidth) then
                                begin MinValue:=SnapshotsForm.MinimumClientWidth;  MaxValue:=Max(MinValue,Screen.DeskTopWidth);
                                end
                             else if  (Setting=stWindowsSnapshotsHeight) or (Setting=stWindowsInternalClipboardHeight) then
                                begin MinValue:=SnapshotsForm.MinimumClientHeight; MaxValue:=Max(MinValue,Screen.DeskTopHeight);
                                end
                             else if  Setting=stWindowsOpenSolutionColumns then
                                begin MinValue:=0; MaxValue:=2;
                                end
                             else if  (Setting=stToolsSolverTimeLimitSeconds) or
                                      (Setting=stToolsOptimizerTimeLimitSeconds) then
                                begin MinValue:=1; MaxValue:=MAX_PLUGIN_TIME_LIMIT_MS div ONE_THOUSAND;
                                end
{$IFDEF MUSIC_PLAYER}
                             else if  Setting=stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeDelayMS then
                                begin MinValue:=0; MaxValue:=VIEWER_MAX_TIME_DELAY_MS;  Increment:=1000;
                                end
                             else if  Setting=stMusicPlayerDisplayActivitiesActivitiesImageViewerSlideShowTimeFadeMS then
                                begin MinValue:=0; MaxValue:=VIEWER_MAX_FADE_TIME_MS; Increment:=100;
                                end
//
//                           else if  (Setting=stMusicPlayerDisplayActivitiesActivitiesFireworks1ColorRocketsInterval) or
//                                    (Setting=stMusicPlayerDisplayActivitiesActivitiesFireworks2ColorRocketsInterval) then
//                              begin MinValue:=0; MaxValue:=10000; Increment:=100;
//                              end
//
                             else if  (Setting=stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorCyclingInterval) or
                                      (Setting=stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorChangeInterval) or
                                      (Setting=stMusicPlayerDisplayActivitiesActivitiesMandalaEngineColorEffectsInterval) or
                                      (Setting=stMusicPlayerDisplayActivitiesActivitiesMandalaEnginePatternInterval) then
                                begin MinValue:=0; MaxValue:=600000; Increment:=1000;
                                end
{$ENDIF}
                            else if  Setting=stGraphicsBoardReverseModeBackgroundPlayerStartPositionGridSize then
                                begin MinValue:=MIN_PLAYER_START_POSITION_GRID_SIZE; MaxValue:=MAX_PLAYER_START_POSITION_GRID_SIZE;
                                end

                             else if  (Setting=stGraphicsBoardFiguresWallCapLeft) or
                                      (Setting=stGraphicsBoardFiguresWallCapTop) then
                                begin MinValue:=-1; MaxValue:=100; // // kludge: '-1' = offset 0; '0' = default offset = midways between the squares
                                end

                             else if  Setting=stGraphicsBoardFiguresMaxZoomFactorPct then
                                begin MinValue:=MIN_ZOOM_FACTOR_PCT;
                                      MaxValue:=MAX_ZOOM_FACTOR_PCT;
                                end

                             else if  Setting=stGraphicsStatusSubTextsFontSize then
                                begin MinValue:=MIN_STATUS_PANEL_SUBTEXT_FONT_SIZE;
                                      MaxValue:=MAX_STATUS_PANEL_SUBTEXT_FONT_SIZE;
                                end
                             else  if (Setting>=stGraphicsToolsEditorCursors) and
                                      (Setting<=stGraphicsToolsEditorCursorsSelectionCursorShadowColor) then
                                begin MinValue:=1; MaxValue:=Tools_.MAX_CURSOR_PEN_WIDTH;
                                end;

                             ShowControl(SpinEdit1,True);
                       end;
    seColor        : ShowControl(SelectBtn,True); //ShowControl(Panel1,True);
    seMPlayerColor : FillComboBox(Ord(Low(MusicPlayerColorText)),Ord(High(MusicPlayerColorText)),MusicPlayerColorText);
    seFont         : ShowControl(SelectBtn,True); // ShowControl(Panel1,True);
    seSolution     : FillComboBox(Ord(Low(SolutionTypeText)),Ord(High(SolutionTypeText)),SolutionTypeText);
    seOptimization : FillComboBox(Ord(Low(PathFindingOptimizationTypeText)),Ord(High(PathFindingOptimizationTypeText)),PathFindingOptimizationTypeText);
    seSoundFileName,
    sePlayListItem,
    seImageFileName,
    seFilePath,
    seMusicPlayListFileName,
    sePaletteFileName,
    seSolverFileName,
    seOptimizerFileName
                   : ShowControl(SelectBtn,True); // ShowControl(Panel1,True);
    seMusicSource  : FillComboBox(Ord(Low(MusicSourceText)),Ord(High(MusicSourceText)),MusicSourceText);
    seImageRect    : ShowControl(SelectBtn,True); // ShowControl(Panel1,True);
    seImageView    : if   Setting=stGraphicsBackgroundImageView then
                          FillComboBox(Ord(Low(ImageViewText)),     Ord(High(ImageViewText)) ,ImageViewText)
                     else FillComboBox(Ord(Low(ImageViewText)),Pred(Ord(High(ImageViewText))),ImageViewText); // all except "Floor tile"
    seDrive        : begin // used only for CD Drive at the moment:
                       GetDrivesOfType(DRIVE_REMOVABLE,DRIVE_CDROM,ComboBox1);
                       ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(StringGrid1.Cells[0,EditorRow]);
                       ShowControl(ComboBox1,True);
                     end;
    seAntiAliasing : FillComboBox(Ord(Low(AntiAliasingText                    )),Ord(High(AntiAliasingText                    )),AntiAliasingText);
    seTitleMode    : FillComboBox(Ord(Low(MusicPlayerTitleModeText            )),Ord(High(MusicPlayerTitleModeText            )),MusicPlayerTitleModeText);
    seActivity     : FillComboBox(Ord(Low(ActivityText                        )),Ord(High(ActivityText                        )),ActivityText);
    seResizeToWindowWidth
                   : FillComboBox(Ord(Low(Viewer1ResizeToWindowWidthText      )),Ord(High(Viewer1ResizeToWindowWidthText      )),Viewer1ResizeToWindowWidthText);
    seOnEscapeKeyAction
                   : FillComboBox(Ord(Low(Text_.OnEscapeKeyActionText         )),Ord(High(Text_.OnEscapeKeyActionText         )),Text_.OnEscapeKeyActionText);
    seOnLeftClickEmptySquareAction
                   : FillComboBox(Ord(Low(OnLeftClickEmptySquareActionText    )),Ord(High(OnLeftClickEmptySquareActionText    )),OnLeftClickEmptySquareActionText);
    seOnRightClickAction
                   : FillComboBox(Ord(Low(OnRightClickActionText              )),Ord(High(OnRightClickActionText              )),OnRightClickActionText);
    seOnShiftKeyAction
                   : FillComboBox(Ord(Low(OnShiftKeyActionText                )),Ord(High(OnShiftKeyActionText                )),OnShiftKeyActionText);
    seCursorType   : FillComboBox(Ord(Low(CursorTypeText                      )),Ord(High(CursorTypeText                      )),CursorTypeText);
    seBoardDimensionsAsText
                   : FillComboBox(Ord(Low(Text_.BoardDimensionsAsText         )),Ord(High(Text_.BoardDimensionsAsText         )),Text_.BoardDimensionsAsText);
    seDeadlockDetectionType
                   : FillComboBox(Ord(Low(Text_.DeadlockDetectionTypeText     )),Ord(High(Text_.DeadlockDetectionTypeText     )),Text_.DeadlockDetectionTypeText);
    seDisplayDriverChangedAction
                   : FillComboBox(Ord(Low(Text_.DisplayDriverChangedActionText)),Ord(High(Text_.DisplayDriverChangedActionText)),Text_.DisplayDriverChangedActionText);
    seWallType     : FillComboBox(Ord(Low(WallTypeText                        )),Ord(High(WallTypeText                        )),WallTypeText);
    seInBetweenFramesType
                   : FillComboBox(Ord(Low(InBetweenFramesTypeText             )),Ord(High(InBetweenFramesTypeText             )),InBetweenFramesTypeText);
    seFloorFillCharacter
                   : FillComboBox(Ord(Low(FloorFillCharacter                  )),Ord(High(FloorFillCharacter                  )),FloorFillCharacter);
    sePluginThreadPriority
                   : FillComboBox(Ord(Low(TPluginThreadPriority               )),Ord(High(TPluginThreadPriority               )),ThreadPriorityText);

    seAlternatingOptimizations
                   : FillComboBox(Ord(Low(TAlternatingOptimizations           )),Ord(High(TAlternatingOptimizations           )),AlternatingOptimizationsText);
    seAlternatingOptimizationsTrigger
                   : FillComboBox(Ord(Low(Text_.AlternatingOptimizationsTriggerTypeText)),Ord(High(Text_.AlternatingOptimizationsTriggerTypeText)),Text_.AlternatingOptimizationsTriggerTypeText);
    seAlternatingOptimizationsRepeatType
                   : FillComboBox(Ord(Low(Text_.AlternatingOptimizationsRepeatTypeText)),Ord(High(Text_.AlternatingOptimizationsRepeatTypeText)),Text_.AlternatingOptimizationsRepeatTypeText);
    seSkinExportFormatType
                   : FillComboBox(Ord(Low(Text_.SkinExportFormatTypeText)),Ord(High(Text_.SkinExportFormatTypeText)),Text_.SkinExportFormatTypeText);
  end; // case
end;

function  TOptionsForm.HideEditors:Boolean;
begin
  Result:=Edit1.Visible or ComboBox1.Visible or SpinEdit1.Visible;
  Edit1.Hide; ComboBox1.Hide; SpinEdit1.Hide; //Panel1.Hide;
  if ActiveControl<>SelectBtn then SelectBtn.Hide;
//if ActiveControl<>Panel1 then Panel1.Hide;
end;

function  TOptionsForm.CloseEditors(HideFirst:Boolean):Boolean;
begin
  Result:=True;
  if Edit1.Visible then begin
     if HideFirst then Edit1.Hide;
     Result:=UpdateSettings(EditorRow,Edit1.Text,Edit1);
     end;
  if ComboBox1.Visible then begin
     if HideFirst then ComboBox1.Hide;
     Result:=UpdateSettings(EditorRow,ComboBox1.Text,ComboBox1);
     end;
  if SpinEdit1.Visible then begin
     if HideFirst then SpinEdit1.Hide;
     Result:=UpdateSettings(EditorRow,SpinEdit1.Text,SpinEdit1);
     end;
  if Result then HideEditors;
end;

function  TOptionsForm.UpdateSettings(ARow:Integer; s:String; Sender:TObject):Boolean;
var i:Integer; No:TSettings;

  procedure ProtectBusyPlugin(Plugin__:TPlugin; No__:TSettings; var NewValue__:String);
  begin
    if Assigned(Plugin__) then with Plugin__ do begin
       if StrEqual(NewValue__,DefaultPluginFileName) then
          NewValue__:=DEFAULT_VALUE;
       if not StrEqual(NewValue__,SettingsString[No__]) and
          (ThreadState<>ptsIdle) then begin
           NewValue__:=PluginName;
           if   NewValue__= '' then
                NewValue__:=ExtractFileNameWithoutPathAndExtension(PluginFileName);
           if   NewValue__<>'' then
                NewValue__:=Format(PluginIsBusyAndCannotChangeText__,[NewValue__])
           else NewValue__:=ApplicationIsBusyText;
           Msg(NewValue__,Caption+SUB_TITLE_SEPARATOR+ChangingPluginText,MB_OK+MB_ICONERROR);
           NewValue__:=SettingsString[No__]; // return the current value
           end;
       end;
  end;

begin // UpdateSettings
  s:=Trim(s);
  if   ARow>=0 then
       No:=RowToSettings(ARow)
  else No:=TSettings(Abs(ARow)); // negative numbers force updates of items thay may be invisible

  if      SettingsEditor[No]=seFileTypeFilter then
          s:=InternalToExternalFileTypeFilter(ExternalToInternalFileTypeFilter(s))
{$IFDEF MUSIC_PLAYER}
  else if No=stMusicFilePath          then s:=StrWithoutTrailingPathDelimiter(s)
{$ENDIF}
  else if No=stToolsSolverFileName    then ProtectBusyPlugin(MainForm.Solver ,No,s)
  else if No=stToolsOptimizerFileName then ProtectBusyPlugin(MainForm.Optimizer,No,s);

  Result:=AnsiCompareText(s,SettingsString[No])=0;

  if not Result then
     begin
       case SettingsEditor[No] of

         seText                  : if   (No>=stGraphicsSnapshotsTextsSolution) and
                                        (No<=stGraphicsSnapshotsTextsReverseModeSnapshotName) then
                                        Result:=(s<>'') and
                                                (StrAnsiPosCI(TEXT_BUILT_IN,s)=0) and
                                                (StrAnsiPosCI(SNAPSHOT_TYPE_NAME[stSaveGame],s)=0)

                                   else Result:=True;
         sePct,
         seSpin                  : if SafeStrToInt(s,False,i)
                                      and
                                      ( (Sender=nil)
                                        or
                                        (Sender=MainForm.Skins)
                                        or
                                        ((Sender is TSpinEdit) and
                                         (i>=TSpinEdit(Sender).MinValue) and
                                         (i<=TSpinEdit(Sender).MaxValue)
                                        )
                                      ) then
                                      begin s:=IntToStr(i); Result:=True;
                                      end;
         seColor                 : if  SafeStrToInt(RGB_BGR(Copy(StrHexDigits(s),1,6)),True,i) then begin
                                       if TSettings(No)=stGraphicsStatusPanelsColor then
                                          i:=Integer(NotBlackColor(TColor(i)));
                                       s:=RGB_BGR(Misc_.IntToHex(i,6));
                                       Result:=True;
                                       end;
         seBoolean               : Result:=StrIndexOfCI(s,BooleanText)<>-1;
         seSolution              : Result:=StrIndexOfCI(s,SolutionTypeText)<>-1;
         seOptimization          : Result:=StrIndexOfCI(s,PathFindingOptimizationTypeText)<>-1;
         seMusicSource           : Result:=StrIndexOfCI(s,MusicSourceText)<>-1;
         seImageView             : begin
                                     i:=StrIndexOfCI(s,ImageViewText);
                                     Result:=(i<>-1)
                                             and
                                             ((i<>Ord(ivFloorTile))
                                              or
                                              (No=stGraphicsBackgroundImageView));
                                   end;
         seDrive                 : if Sender=nil then Result:=True
                                   else if (Sender=ComboBox1) or (Sender=MainForm.Skins) then begin
                                           s:=AnsiUpperCase(s);
                                           if Length(s)=1 then s:=s+COLON;
                                           Result:=ComboBox1.Items.IndexOf(s)<>-1;
                                           end;
         seAntiAliasing          : begin
                                     Result:=StrIndexOfCI(s,AntiAliasingText)<>-1;
                                     if not Result then begin
                                        i:=StrIndexOfCI(s,Antialiasing3Text); // "Antialiasing3Text" is never then "Antialiasing2Text", hence "Antialiasing3Text" that first
                                        Result:=i<>-1;
                                        if Result then
                                           s:=AntialiasingText[TAntialiasing(i)]
                                        else begin
                                           i:=StrIndexOfCI(s,Antialiasing2Text);
                                           Result:=i<>-1;
                                           if Result then
                                              s:=AntialiasingText[TAntialiasing(i)]
                                           end;
                                        end;
                                   end;
         seMPlayerColor          : Result:=StrIndexOfCI(s,MusicPlayerColorText)<>-1;
         seTitleMode             : Result:=StrIndexOfCI(s,MusicPlayerTitleModeText)<>-1;
         seActivity              : Result:=StrIndexOfCI(s,ActivityText)<>-1;
         seResizeToWindowWidth   : Result:=StrIndexOfCI(s,Viewer1ResizeToWindowWidthText)<>-1;
         seOnEscapeKeyAction     : Result:=StrIndexOfCI(s,OnEscapeKeyActionText)<>-1;
         seOnRightClickAction    : Result:=StrIndexOfCI(s,OnRightClickActionText)<>-1;
         seCursorType            : Result:=StrIndexOfCI(s,CursorTypeText)<>-1;
         seDeadlockDetectionType : Result:=StrIndexOfCI(s,DeadlockDetectionTypeText)<>-1;
         seWallType              : begin Result:=(StrIndexOfCI(s,WallTypeText)<>-1);
                                         if not Result then begin
                                            i:=StrIndexOfCI(s,OldWallTypeText); // walltype texts changed in version 1.309
                                            if i>=0 then begin
                                               s:=WallTypeText[TWallType(i)];
                                               Result:=True;
                                               end;
                                            end;
                                   end;
         seInBetweenFramesType   : Result:=StrIndexOfCI(s,InBetweenFramesTypeText)<>-1;
         seFloorFillCharacter    : Result:=StrIndexOfCI(s,FloorFillCharacter)<>-1;
         sePluginThreadPriority  : begin i:=StrIndexOfCI(s,ThreadPriorityText);
                                         Result:=(i>=Ord(Low(TPluginThreadPriority))) and (i<=Ord(High(TPluginThreadPriority)));
                                   end;
         seAlternatingOptimizations
                                 : Result:=StrIndexOfCI(s,AlternatingOptimizationsText)<>-1;
         seAlternatingOptimizationsTrigger
                                 : Result:=StrIndexOfCI(s,AlternatingOptimizationsTriggerTypeText)<>-1;
         seAlternatingOptimizationsRepeatType
                                 : Result:=StrIndexOfCI(s,AlternatingOptimizationsRepeatTypeText)<>-1;
         seSkinExportFormatType  : Result:=StrIndexOfCI(s,SkinExportFormatTypeText)<>-1;
         else                      Result:=True;
       end; // case

       if Result then begin
          Self.Modified:=True;
          if Sender<>MainForm.Skins then ShowStatus;

          if (TSettings(No)>=stGraphicsBoard) and (TSettings(No)<stGraphicsMenu) then
             BoardGraphicsModified:=True;

          if (SettingsEditor[No]=seImageFileName) and
             ((Sender=Edit1) or (Sender=MainForm.Skins)) and
             (AnsiCompareText(s,DEFAULT_VALUE)=0) then begin
             s:=DEFAULT_VALUE;
             if AnsiCompareText(ImageSectionText,TreeView1.Items[Succ(Ord(No))].Text)=0 then // if a rectangle follows...
                SetToDefaultValue(TreeView1.Items[Succ(Ord(No))]);
             if (TSettings(No)=stGraphicsBoardFiguresWallImageFileName) then
                for i:=Ord(stGraphicsBoardFiguresWallImageMasked) to Ord(stGraphicsBoardFiguresWallOuterWallTrimmingBottom) do
                    SetToDefaultValue(TreeView1.Items[i]);
             end;

          if (Sender<>MainForm.Skins)
             and
             ((TSettings(No)=stGraphicsBackgroundImageFileName)
              or
              (TSettings(No)=stGraphicsBoardBackgroundImageFileName)
             )
             and
             (AnsiCompareText(SettingsString[No],DEFAULT_VALUE)=0)
             then begin
             if (TSettings(No)=stGraphicsBackgroundImageFileName) and
                (AnsiCompareText(SettingsString[stGraphicsBackgroundImageAntialiasing],AntialiasingText[aaBilinear])=0) then begin
                FSettingsString[stGraphicsBackgroundImageAntialiasing]:=AntialiasingText[aaFilter];
                with TreeView1.Items[Ord(stGraphicsBackgroundImageAntialiasing)] do
                  if Data<>nil then StringGrid1.Cells[0,Integer(Data)]:=SettingsString[stGraphicsBackgroundImageAntialiasing];
                end;
             if (TSettings(No)=stGraphicsBoardBackgroundImageFileName) and
                (AnsiCompareText(SettingsString[stGraphicsBoardBackgroundImageAntialiasing],AntialiasingText[aaBilinear])=0) then begin
                FSettingsString[stGraphicsBoardBackgroundImageAntialiasing]:=AntialiasingText[aaFilter];
                with TreeView1.Items[Ord(stGraphicsBoardBackgroundImageAntialiasing)] do
                  if Data<>nil then StringGrid1.Cells[0,Integer(Data)]:=SettingsString[stGraphicsBoardBackgroundImageAntialiasing];
                end;
             end;

          if (TSettings(No)=stToolsSolverFileName) and
             //(AnsiCompareText(ChangeFileExt(ExtractFileName(s),''),DEFAULT_SOLVER_FILE_NAME_STUB)=0) and
             Assigned(MainForm.Solver) and
             //(MainForm.Solver.ThreadState=ptsIdle)
             StrEqual(s,MainForm.Solver.DefaultPluginFileName) then
             s:=DEFAULT_VALUE;

          if (TSettings(No)=stToolsOptimizerFileName) and
             // (AnsiCompareText(ChangeFileExt(ExtractFileName(s),''),DEFAULT_OPTIMIZER_FILE_NAME_STUB)=0) and
             Assigned(MainForm.Optimizer) and
             // (MainForm.Optimizer.ThreadState=ptsIdle)
             StrEqual(s,MainForm.Optimizer.DefaultPluginFileName) then
             s:=DEFAULT_VALUE;

          FSettingsString[No]:=s;
          if Sender<>MainForm.Skins then begin
             if      ARow>=0 then StringGrid1.Cells[0,ARow]:=SettingsString[No]
             else if (Ord(No)<TreeView1.Items.Count) and
                     (TreeView1.Items[Ord(No)].Data<>nil) then
                     StringGrid1.Cells[0,Integer(TreeView1.Items[Ord(No)].Data)]:=SettingsString[No];

             if (TSettings(No)>=stGraphicsBoardFiguresWallImageSectionLeft) and
                (TSettings(No)<=stGraphicsBoardFiguresWallImageSectionHeight) and
                (SettingsString[stGraphicsBoardFiguresWallImageFileName]=DEFAULT_VALUE) then begin
                if      HasDefaultSkinWall then begin // 'True': the default wall tile is now selected; adjust the settings accordingly
                        for i:=Ord(stGraphicsBoardFiguresWallImageMasked) to Ord(stGraphicsBoardFiguresWallOuterWallTrimmingBottom) do
                            SetToDefaultValue(TreeView1.Items[i]);
                        MenuItemDefaultSkinSeamlessWalls.Checked :=True;
                        MenuItemDefaultSkinTiledWalls   .Checked :=not MenuItemDefaultSkinSeamlessWalls.Checked;
                        MenuItemDefaultSkinBrickWalls   .Checked :=False;
                        end
                else if HasDefaultSkinBrickWall then begin
                        for i:=Ord(stGraphicsBoardFiguresWallImageMasked) to Ord(stGraphicsBoardFiguresWallOuterWallTrimmingBottom) do
                            SetToDefaultValue(TreeView1.Items[i]);
                        UpdateSettings(-Ord(stGraphicsBoardFiguresWallImageAntiAliasing),AntiAliasingText[aaFilter],Sender);
                        MenuItemDefaultSkinSeamlessWalls.Checked :=False;
                        MenuItemDefaultSkinTiledWalls   .Checked :=False;
                        MenuItemDefaultSkinBrickWalls   .Checked :=True;
                        end;
                end;

             if (No=stGraphicsBoardFiguresWallType) and HasDefaultSkinWall then begin
                MenuItemDefaultSkinSeamlessWalls.Checked:=not StrEqual(SettingsString[stGraphicsBoardFiguresWallType],WallTypeText[wtTiledWall]);
                MenuItemDefaultSkinTiledWalls   .Checked:=not MenuItemDefaultSkinSeamlessWalls.Checked;
                MenuItemDefaultSkinBrickWalls   .Checked:=False;
                end;
             end;
          end
       else begin
          if   s='' then
               s:=Format(InvalidProperty1ValueText,[FullItemName(TreeView1.Items[Ord(No)])])
          else s:=Format(InvalidProperty2ValueText,[FullItemName(TreeView1.Items[Ord(No)]),s]);
          Msg(s,Caption,MB_OK+MB_ICONERROR);

          if ARow>=0 then begin
             s:=StringGrid1.Cells[0,ARow];
             if      (Sender=Edit1)     and Edit1    .Visible then
                     begin Edit1.Text:=s; Edit1.SetFocus end
             else if (Sender=ComboBox1) and ComboBox1.Visible then
                     begin ComboBox1.Text:=s; ComboBox1.SetFocus; end
             else if (Sender=SpinEdit1) and SpinEdit1.Visible then
                     begin SpinEdit1.Value:=StrToInt(s); SpinEdit1.SetFocus;
                     end;
             end;
          end;
     end;
end;

function  TOptionsForm.GetFontSettings(Index:Integer; var FontIndex:Integer; var Font__:TFont):Boolean;
var i:Integer;
begin
  FontIndex:=Index;
  while (FontIndex>=0) and (not StrEndsWith(TreeView1.Items[FontIndex].Text,FontText)) do
    Dec(FontIndex);

  Result:=(ScratchFont<>nil) and (FontIndex>=0);
  if Result then begin
     Font__:=ScratchFont;
     Font__.Name:=SettingsString[TSettings(FontIndex+1)];
     if not SafeStrToInt(SettingsString[TSettings(FontIndex+2)],False,i) then i:=10;
     Font__.Size:=i;
//   Font__.Color:=StrToInt(RGB_BGR(SettingsString[TSettings(FontIndex+3)]));
     Font__.Style:=[];
     if SettingsString[TSettings(FontIndex+4)]=BooleanText[True] then
        Font__.Style:=Font__.Style+[fsBold];
     if SettingsString[TSettings(FontIndex+5)]=BooleanText[True] then
        Font__.Style:=Font__.Style+[fsItalic];
     if SettingsString[TSettings(FontIndex+6)]=BooleanText[True] then
        Font__.Style:=Font__.Style+[fsUnderline];
     end;
end;

procedure TOptionsForm.SetFontSettings(FontIndex:Integer; Font__:TFont);
var Row:Integer;
begin
  Row:=Integer(TreeView1.Items[FontIndex].Data);
  UpdateSettings(Row+1,Font__.Name,nil);
  UpdateSettings(Row+2,IntToStr(Font__ .Size),nil);
//UpdateSettings(Row+3,IntToStr(Font__.Color),nil);
  UpdateSettings(Row+4,BooleanText[fsBold      in Font__.Style],nil);
  UpdateSettings(Row+5,BooleanText[fsItalic    in Font__.Style],nil);
  UpdateSettings(Row+6,BooleanText[fsUnderline in Font__.Style],nil);
end;

procedure TOptionsForm.Panel1Click(Sender: TObject);
var i,FontIndex,ImageIndex,ColorIndex,oFilterIndex:Integer;
    Ok,FixedDefaultImageSize,SelectionEnabled:Boolean; Font:TFont;
    Task:TOpenTask; SubTask:TOpenSubTask; p:TPictureType;
    Node:TTreeNode;
    DefaultPath,FileName,Filter,FormCaption,ImageFileExt,TopPanelCaption,s,s1:String;
    oOnHint:TNotifyEvent; oOnMessage:TMessageEvent; ARect,DefaultRect:TRect; Color:TColor;
    oFilter,oTitle:String; Editor:TSettingsEditor;

  procedure OpenDialog(const Title__,Filter__:String);
  var oFilterIndex:Integer; s,s1,oFilter,oTitle:String;
  begin
    with OpenDialog1 do begin
      oTitle:=Title; oFilter:=Filter; oFilterIndex:=FilterIndex;
      try Title:=Self.Caption+SUB_TITLE_SEPARATOR+Title__;
          Filter:=Filter__+BAR+AllFilesFilterText;
          FilterIndex:=1;
          s:=SettingsString[RowToSettings(EditorRow)];
          s1:=StrWithoutTrailingPathDelimiter(ExtractFilePath(s));
          if DirectoryExists(s1) then begin
             InitialDir:=s1; FileName:=ExtractFileName(s);
             end
          else begin
             InitialDir:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
             FileName:='';
             end;

          if Execute then
             begin HideEditors;
                   UpdateSettings(EditorRow,FileName,Self);
                   ShowEditor(EditorRow);
             end;

      finally Title:=oTitle; Filter:=oFilter; FilterIndex:=oFilterIndex;
      end;
      end;
  end;

begin // Panel1Click
  if (not IsBusy) then
     begin //SelectBtn.Hide;
           if SelectBtn.Visible {Panel1.Visible} then begin
              SelectBtn.Hide; //Panel1.Hide;
              HideEditors; ActiveControl:=StringGrid1; Update;
              end;
           ARect:=Rect(-1,-1,-1,-1); FillChar(DefaultRect,SizeOf(DefaultRect),0);
           ImageIndex:=-1; ColorIndex:=-1; Color:=TColor(-1);
           FixedDefaultImageSize:=True; SelectionEnabled:=True; SubTask:=osNone;

           if   (Sender=MainForm) and Assigned(TreeView1.Selected) then begin
                Node:=TreeView1.Selected.Parent;
                while Assigned(Node) and (not Node.Expanded) do begin
                  Node.Expand(False); Node:=Node.Parent;
                  end;
                TreeToGrid;
                EditorRow:=Integer(TreeView1.Selected.Data);
                Edit1.Text:=StringGrid1.Cells[0,EditorRow];
                end;

           Editor:=SettingsEditor[RowToSettings(EditorRow)];

           if (Editor=seFont) and
              GetFontSettings(Integer(RowToSettings(EditorRow)),FontIndex,Font) then
              begin
                    FontDialog1.Font.Assign(Font);
                    if FontDialog1.Execute and
                       (not FontsIdentical(FontDialog1.Font,Font)) then
                       begin SetFontSettings(FontIndex,FontDialog1.Font);
                             Modified:=True;
                             ShowStatus;
                             HideEditors;
                             ShowEditor(EditorRow);
                       end
                    else EscapeEnabled:=False;
              end
           else if Editor=seColor then
              begin Ok:=SafeStrToInt(RGB_BGR(Copy(Edit1.Text,1,6)),True,i);
                    if not Ok then i:=Integer(clYellow);
                    ColorDialog1.Color:=TColor(i);
                    try IsBusy:=True;
                        if ColorDialog1.Execute and
                           ((ColorDialog1.Color<>TColor(i)) or (not Ok)) then
                           begin HideEditors;
                                 UpdateSettings(EditorRow,RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(ColorDialog1.Color),6)),Self);
                                 ShowEditor(EditorRow);
                           end
                        else EscapeEnabled:=False;
                        Application.ProcessMessages;
                    finally IsBusy:=False;
                    end;
              end
           else if Editor=seMusicPlayListFileName then
                   with OpenDialog1 do begin
                     oTitle:=Title; oFilter:=Filter; oFilterIndex:=FilterIndex;
                     try Title:=Self.Caption+SUB_TITLE_SEPARATOR+PlayListText;
                         Filter:=PlayListsText+' (*.'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+')|*'+Copy(PLAYLIST_FILE_EXT,2,MaxInt)+BAR+AllFilesFilterText;
                         FilterIndex:=1;
                         s:=SettingsString[RowToSettings(EditorRow)];
                         s1:=StrWithoutTrailingPathDelimiter(ExtractFilePath(s));
                         if DirectoryExists(s1) then begin
                            InitialDir:=s1; FileName:=ExtractFileName(s);
                            end
                         else begin
                            InitialDir:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
                            FileName:='';
                            end;

                         if Execute then
                            begin HideEditors;
                                  UpdateSettings(EditorRow,FileName,Self);
                                  ShowEditor(EditorRow);
                            end
                         else EscapeEnabled:=False;

                    finally Title:=oTitle; Filter:=oFilter; FilterIndex:=oFilterIndex;
                    end;
              end
           else if (Editor in
                     [seSoundFileName,sePlayListItem,seFilePath,seImageFileName,seImageRect,sePaletteFileName,seSolverFileName,seOptimizerFileName])
                   and
                   (TreeView1.Selected<>nil)
                   and
                   IsOpenFormAvailable
                   then
              begin FileName:=Edit1.Text;
                    ImageFileExt:=BMP_FILE_EXT;
                    DefaultPath:=MainForm.ApplicationDataPath;

                    case SettingsEditor[RowToSettings(EditorRow)] of
                      seSoundFileName: begin Task:=otSound; Node:=TreeView1.Selected.Parent;
                                             Filter:=Mainform.Sound.SoundFileTypeFilter;
                                             //Filter:=ExternalToInternalFileTypeFilter(SettingsString[stSoundFileTypeFilter]);
                                             s:=StrWithTrailingPathDelimiter(DefaultPath)+DEFAULT_VALUE+WAV_FILE_EXT;
                                             if AnsiCompareText(SettingsString[RowToSettings(EditorRow)],DEFAULT_VALUE)=0 then
                                                FileName:=s;
                                             if not ResourceSaveToFile(s,
                                                                       // caution : 'Move' and 'Push' must be the first 2 sounds
                                                                       SOUND_RES_NAME[TSoundType((Ord(RowToSettings(EditorRow))-Ord(stSoundMoveFileName)) div (Ord(stSoundPush)-Ord(stSoundMove)))],
                                                                       RC_WAV) then
                                                raise Exception.Create(Format(SaveFileFailedText__,[s]));

                                       end;
                      sePlayListItem : begin Task:=otMusic; Node:=TreeView1.Selected;
                                             {$IFDEF MUSIC_PLAYER}
                                               Filter:=ExternalToInternalFileTypeFilter(SettingsString[stMusicFileTypeFilter]);
                                             {$ENDIF}
                                       end;
                      seFilePath     : begin Task:=otMusicPath; Node:=TreeView1.Selected;
                                             {$IFDEF MUSIC_PLAYER}
                                                Filter:=ExternalToInternalFileTypeFilter(SettingsString[stMusicFileTypeFilter]);
                                             {$ENDIF}
                                       end;
                      sePaletteFileName
                                     : begin Task:=otPalette; Node:=TreeView1.Selected;
                                             Filter:=PALETTE_FILES_FILTER;
                                             ImageIndex:=Integer(RowToSettings(EditorRow));
                                             if AnsiCompareText(DEFAULT_VALUE,FileName)=0 then
                                                FileName:=Fractals.DefaultPaletteFileName;
                                       end;
                      seSolverFileName
                                     : begin Task:=otSolver; Node:=TreeView1.Selected;
                                             Filter:=PLUGIN_FILES_FILTER;
                                             if StrEqual(DEFAULT_VALUE,FileName)
                                                or
                                                (not FileExists(FileName)) then
                                                FileName:=MainForm.Solver.DefaultPluginFileName;
                                       end;
                      seOptimizerFileName
                                     : begin
                                             Task:=otOptimizer; Node:=TreeView1.Selected;
                                             Filter:=PLUGIN_FILES_FILTER;
                                             if StrEqual(DEFAULT_VALUE,FileName)
                                                or
                                                (not FileExists(FileName)) then
                                                FileName:=MainForm.Optimizer.DefaultPluginFileName;
                                       end;
                      else             begin Task:=otImage;
                                             if Editor=seImageFileName then begin
                                                {$IFDEF MUSIC_PLAYER}
                                                   if   RowToSettings(EditorRow)=stMusicPlayerDisplayActivitiesActivitiesImageViewerFileName then
                                                        Node:=TreeView1.Selected
                                                   else Node:=TreeView1.Selected.Parent.Parent;
                                                {$ELSE}
                                                  Node:=TreeView1.Selected.Parent.Parent;
                                                {$ENDIF}
                                                ImageIndex:=Integer(RowToSettings(EditorRow));
                                                end
                                             else begin
                                                Node:=TreeView1.Selected.Parent.Parent.Parent;
                                                ImageIndex:=Pred(TreeView1.Selected.Parent.AbsoluteIndex);
                                                FileName:=SettingsString[TSettings(ImageIndex)];
                                                end;

                                             Filter:=IMAGE_FILES_FILTER;
                                             if (ImageIndex+5<TreeView1.Items.Count) and // if a rectangle follows...
                                                (AnsiCompareText(ImageSectionText,TreeView1.Items[Succ(ImageIndex)].Text)=0) then begin
                                                if   SafeStrToInt(SettingsString[TSettings(ImageIndex+2)],False,ARect.Left  ) and
                                                     SafeStrToInt(SettingsString[TSettings(ImageIndex+3)],False,ARect.Top   ) and
                                                     SafeStrToInt(SettingsString[TSettings(ImageIndex+4)],False,ARect.Right ) and
                                                     SafeStrToInt(SettingsString[TSettings(ImageIndex+5)],False,ARect.Bottom) then
                                                     with ARect do begin
                                                       Inc(ARect.Right,ARect.Left); // Right :=Left+Width
                                                       Inc(ARect.Bottom,ARect.Top); // Bottom:=Top+Height
                                                       end
                                                else ARect:=Rect(0,0,0,0);
                                                i:=ImageIndex+7;
                                                end
                                            else i:=Succ(ImageIndex);
                                            if (i<TreeView1.Items.Count) and
                                               (AnsiCompareText(BackgroundColorText,TreeView1.Items[i].Text)=0) then begin
                                               ColorIndex:=i;
                                               if   SafeStrToInt(RGB_BGR(SettingsString[TSettings(i)]),True,i) then
                                                    Color:=TColor(i)
                                               else Color:=Graphics.ColorToRGB(clBlack);
                                               end;
                                            try
                                              s:=StrWithTrailingPathDelimiter(DefaultPath)+DEFAULT_VALUE+BMP_FILE_EXT;
                                              if      (TSettings(ImageIndex)=stGraphicsBackgroundImageFileName)
                                                      or
                                                      (TSettings(ImageIndex)=stGraphicsToolsBoardBackgroundImageFileName)
                                                      then begin
                                                      ImageFileExt:=JPG_FILE_EXT;
                                                      s:=ChangeFileExt(s,ImageFileExt);
                                                      if not ResourceSaveToFile(s,BACKGROUND_RES_NAME,RC_JPG) then
                                                         raise Exception.Create(Format(SaveFileFailedText__,[s]));
                                                      FixedDefaultImageSize:=False;
                                                      end
                                              else if TSettings(ImageIndex)=stGraphicsBoardBackgroundImageFileName then begin
                                                      ImageFileExt:=JPG_FILE_EXT;
                                                      s:=ChangeFileExt(s,ImageFileExt);
                                                      if not ResourceSaveToFile(s,BOARD_BACKGROUND_RES_NAME,RC_JPG) then
                                                         raise Exception.Create(Format(SaveFileFailedText__,[s]));
                                                      FixedDefaultImageSize:=False;
                                                      end
                                              else if TSettings(ImageIndex)=stGraphicsBoardReverseModeBackgroundImageFileName then begin
                                                      ImageFileExt:=BMP_FILE_EXT;
                                                      s:=ChangeFileExt(s,ImageFileExt);
                                                      try    MainForm.ReverseModeBackgroundImage.Picture.BitMap.SaveToFile(s);
                                                      except on E:Exception do raise Exception.Create(Format(SaveFileFailedText__,[s])+NL+NL+E.Message);
                                                      end;
                                                      FixedDefaultImageSize:=False;
                                                      end
{$IFDEF MUSIC_PLAYER}
                                              else if TSettings(ImageIndex)=stMusicPlayerDisplayActivitiesActivitiesImageViewerFileName then begin
                                                      ImageFileExt:=JPG_FILE_EXT;
                                                      s:=ChangeFileExt(s,ImageFileExt);
                                                      if not ResourceSaveToFile(s,BOARD_BACKGROUND_RES_NAME,RC_JPG) then
                                                         raise Exception.Create(Format(SaveFileFailedText__,[s]));
                                                      SelectionEnabled:=False;
                                                      end
{$ENDIF}
                                              else if (TSettings(ImageIndex)>=stGraphicsBoardFigures) and
                                                      (TSettings(ImageIndex)< stGraphicsMenu) then begin
                                                      SubTask:=osTile;
                                                      if TSettings(ImageIndex)>stGraphicsBoardFiguresPlayer then
                                                         for p:=ptPlayer to ptBoxOnGoalAnimation do
                                                             if (ImageIndex>=PictureSettingsOffset[p]) and
                                                                ((p=ptBoxOnGoalAnimation)
                                                                 or
                                                                 (ImageIndex<PictureSettingsOffset[Succ(p)])
                                                                ) then begin
                                                                DefaultRect:=
                                                                  MainForm.GamePictures.DefaultRect[p];
                                                                break;
                                                                end;
                                                      MainForm.DefaultTileSetImage.Picture.SaveToFile(s);
                                                      if (TSettings(ImageIndex)>stGraphicsBoardFiguresWall) and
                                                         (TSettings(ImageIndex)<stGraphicsBoardFiguresFloor) then
                                                         SubTask:=osWall;
                                                      end
                                              else if (TSettings(ImageIndex)>=stGraphicsMenu)  and
                                                      (TSettings(ImageIndex)< stGraphicsStatus) then begin
                                                      MainForm.DefaultButtonSetImage.Picture.SaveToFile(s);
                                                      DefaultRect:=MainForm.Menu.DefaultTileRect[TButtonState((ImageIndex-Ord(stGraphicsMenu)) div (Ord(stGraphicsMenuEnabledButton)-Ord(stGraphicsMenuDisabledButton)))];
                                                      end
                                              else if (TSettings(ImageIndex)>=stGraphicsStatus) and
                                                      (TSettings(ImageIndex)<=stGraphicsStatusTextShadowColor) then begin
                                                      MainForm.DefaultButtonSetImage.Picture.SaveToFile(s);
                                                      DefaultRect:=MainForm.Status.DefaultTileRect;
                                                      end
                                              else if TSettings(ImageIndex)=stGraphicsToolsButtonsImageFileName then begin
                                                      s:=StrWithTrailingPathDelimiter(DefaultPath)+DEFAULT_VALUE+BMP_FILE_EXT;
                                                      if   ToolsForm.SaveDefaultButtonsToFile(s) then
                                                           DefaultPath:=ExtractFilePath(s)
                                                      else raise Exception.Create(Format(SaveFileFailedText__,[s]));
                                                      end
                                              else if TSettings(imageIndex)=stGraphicsToolsBoardSkinImageFileName then begin
                                                      DefaultPath:=ToolsSkinPath;
                                                      s:=StrWithTrailingPathDelimiter(DefaultPath)+DEFAULT_VALUE+BMP_FILE_EXT;
                                                      if FileExists(s) then DeleteFile(s);
                                                      ToolsForm.EditImageDefaultSkin.Picture.BitMap.SaveToFile(s);
                                                      end;
                                            except on E:Exception do Error(E.Message,Caption);
                                            end;
                                            if AnsiCompareText(DEFAULT_VALUE,FileName)=0 then begin
                                               FileName:=StrWithTrailingPathDelimiter(DefaultPath)+DEFAULT_VALUE+ImageFileExt;
                                               if FixedDefaultImageSize and
                                                  (RectWidth (ARect)*RectHeight(ARect)<>0) and
                                                  (RectWidth (ARect)=RectWidth (DefaultRect)) and
                                                  (RectHeight(ARect)=RectHeight(DefaultRect)) then
                                                  DefaultRect:=ARect;
                                               end;
                                       end;
                    end; // case

                    if         Editor=seSolverFileName then begin
                               FormCaption    :=ToolsText+SUB_TITLE_SEPARATOR+SolverText;
                               TopPanelCaption:=TEXT_SOKOBAN+SPACE+AnsiLowerCase(SolverText);
                               end
                    else if    Editor=seOptimizerFileName then begin
                               FormCaption    :=ToolsText+SUB_TITLE_SEPARATOR+OptimizerText;
                               TopPanelCaption:=TEXT_SOKOBAN+SPACE+AnsiLowerCase(OptimizerText);
                               end
                    else begin FormCaption    :=SettingsText;
                               TopPanelCaption:=FullItemName(Node);
                         end;

                    if OpenForm.InitTask(Task,SubTask,FormCaption,TopPanelCaption,FileName,Filter,
                                         StrWithTrailingPathDelimiter(DefaultPath)+DEFAULT_VALUE+ImageFileExt,
                                         ARect,Color,FixedDefaultImageSize,SelectionEnabled,DefaultRect) then begin

                       oOnHint   :=Application.OnHint;
                       oOnMessage:=Application.OnMessage;
                       try
                               if OpenForm.ShowModal=mrOk then
                                  begin HideEditors;
                                        if      Task=otMusicPath then
                                                UpdateSettings(EditorRow,OpenForm.DirectoryListBox1.Directory,Self)
                                        else if Task=otImage then begin
                                                UpdateSettings(-ImageIndex,OpenForm.CurrentFileName,Self);
                                                if (ImageIndex+5<TreeView1.Items.Count) and // if a rectangle follows...
                                                   (AnsiCompareText(ImageSectionText,TreeView1.Items[Succ(ImageIndex)].Text)=0) then begin

                                                   for i:=ImageIndex+2 to ImageIndex+5 do
                                                       if   TreeView1.Items[i].Data<>nil then
                                                            UpdateSettings(Integer(TreeView1.Items[i].Data),IntToStr(OpenForm.GetRectItem(i-ImageIndex-2)),Self)
                                                       else UpdateSettings(-i,IntToStr(OpenForm.GetRectItem(i-ImageIndex-2)),Self);

                                                   if (SubTask=osWall) then begin
                                                      //s:=SettingsString[stGraphicsBoardFiguresWallType];
                                                      //if (OpenForm.GetRectItem(2)<>0) and
                                                      //   (OpenForm.GetRectItem(2 {width})*2=OpenForm.GetRectItem(3 {height})*3) then begin
                                                      //   if   TreeView1.Items[Ord(stGraphicsBoardFiguresWallType)].Data<>nil then
                                                      //        UpdateSettings(Integer(TreeView1.Items[Ord(stGraphicsBoardFiguresWallType)].Data),WallTypeText[wtSeamlessWallWithTop],Self)
                                                      //   else UpdateSettings(-Ord(stGraphicsBoardFiguresWallType),WallTypeText[wtSeamlessWallWithTop],Self);
                                                      //   if   TreeView1.Items[Ord(stGraphicsBoardFiguresWallImageMasked)].Data<>nil then
                                                      //        UpdateSettings(Integer(TreeView1.Items[Ord(stGraphicsBoardFiguresWallImageMasked)].Data),BooleanText[False],Self)
                                                      //   else UpdateSettings(-Ord(stGraphicsBoardFiguresWallImageMasked),BooleanText[False],Self);
                                                      //   end
                                                      //else begin
                                                      //   if   TreeView1.Items[Ord(stGraphicsBoardFiguresWallType)].Data<>nil then
                                                      //        UpdateSettings(Integer(TreeView1.Items[Ord(stGraphicsBoardFiguresWallType)].Data),WallTypeText[wtSingleWall],Self)
                                                      //   else UpdateSettings(-Ord(stGraphicsBoardFiguresWallType),WallTypeText[wtSingleWall],Self);
                                                      //
                                                      //   if   (AnsiCompareText(s,WallTypeText[wtSeamlessWallNoTop  ])=0) or
                                                      //        (AnsiCompareText(s,WallTypeText[wtSeamlessWallWithTop])=0) then
                                                      //        if   TreeView1.Items[Ord(stGraphicsBoardFiguresWallImageMasked)].Data<>nil then
                                                      //             UpdateSettings(Integer(TreeView1.Items[Ord(stGraphicsBoardFiguresWallImageMasked)].Data),BooleanText[True],Self)
                                                      //        else UpdateSettings(-Ord(stGraphicsBoardFiguresWallImageMasked),BooleanText[True],Self);
                                                      //   end;
                                                      end;
                                                   end;

                                                if ColorIndex<>-1 then
                                                   UpdateSettings(-ColorIndex,
                                                                  RGB_BGR(Misc_.IntToHex(Graphics.ColorToRGB(OpenForm.PanelBackgroundColor.Color),6)),
                                                                  Self);

                                                end
                                        else    begin UpdateSettings(EditorRow,OpenForm.CurrentFileName,Self);
                                                end;
                                        ShowEditor(EditorRow);
                                  end
                               else begin // ModalResult <> mrOk
                                  if      (Sender=MainForm) and (Task=otSolver) then    // restore solver filename
                                          MainForm.Solver.PluginFileName:=SettingsString[stToolsSolverFileName]
                                  else if (Sender=MainForm) and (Task=otOptimizer) then // restore optimizer filename
                                          MainForm.Optimizer.PluginFileName:=SettingsString[stToolsOptimizerFileName];
                                  end;
                       finally Application.OnMessage:=oOnMessage;
                               Application.OnHint:=oOnHint;

                               if Task=otImage then
                                  DeleteFile(MainForm.ApplicationDataPath+DEFAULT_VALUE+ImageFileExt)
                               else if Task=otSound then
                                  DeleteFile(MainForm.ApplicationDataPath+DEFAULT_VALUE+WAV_FILE_EXT);

                               if MainForm.ShutDownApplication and (Sender<>MainForm) then
                                  OKBtnClick(Self);
                       end;
                    end;
              end;
     end;
end;

procedure TOptionsForm.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      (Key=VK_RETURN) and
          SelectBtn.Visible and
          //Panel1.Visible and
          (((not (SettingsEditor[RowToSettings(EditorRow)] in [seColor,seSoundFileName,sePlayListItem,seImageFileName]))
           )
           or
           (Edit1.SelLength=Length(Edit1.Text))
          ) then
          // action takes place in 'KeyUp'
          //Panel1Click(Sender)
  else if (Key in [{VK_RETURN,}VK_UP,VK_DOWN{,VK_HOME,VK_END}]) and
          UpdateSettings(EditorRow,Edit1.Text,Edit1) then
          begin StringGrid1.SetFocus;
                if Key<>VK_RETURN then
                   PostMessage(StringGrid1.Handle, WM_KEYDOWN, Key, 0);
                Key:=0;
          end;
end;

procedure TOptionsForm.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not IsBusy then
     if      (Key=VK_RETURN) and
             SelectBtn.Visible and
             //Panel1.Visible and
             (((not (SettingsEditor[RowToSettings(EditorRow)] in [seColor,seSoundFileName,sePlayListItem,seImageFileName]))
              )
              or
              (Edit1.SelLength=Length(Edit1.Text))
             ) then
             Panel1Click(Sender)
     else if (Key in [VK_RETURN{,VK_UP,VK_DOWN{,VK_HOME,VK_END}]) and
             UpdateSettings(EditorRow,Edit1.Text,Edit1) then
             begin StringGrid1.SetFocus;
                   if Key<>VK_RETURN then
                      PostMessage(StringGrid1.Handle, WM_KEYDOWN, Key, 0);
                   Key:=0;
             end
     else if Key=VK_ESCAPE then
             begin Edit1.Text:=StringGrid1.Cells[0,EditorRow];
                   Edit1.SelLength:=Length(Edit1.Text);
                   EscapeEnabled:=False;
                   if SettingsEditor[RowToSettings(EditorRow)]<>seFont then
                      UpdateSettings(EditorRow,Edit1.Text,Edit1);
             end;
end;

procedure TOptionsForm.ComboBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//
end;

procedure TOptionsForm.ComboBox1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      (Key in [VK_RETURN,VK_HOME,VK_END]) and
          UpdateSettings(EditorRow,ComboBox1.Text,ComboBox1) then
          begin StringGrid1.SetFocus;
                if Key<>VK_RETURN then
                   PostMessage(StringGrid1.Handle, WM_KEYDOWN, Key, 0);
                Key:=0;
          end
  else if Key=VK_ESCAPE then
          begin ComboBox1.Text:=StringGrid1.Cells[0,EditorRow];
                ComboBox1.SelLength:=Length(ComboBox1.Text);
                EscapeEnabled:=False;
                UpdateSettings(EditorRow,ComboBox1.Text,ComboBox1);
          end;
end;

procedure TOptionsForm.SpinEdit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//
end;

procedure TOptionsForm.SpinEdit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if      (Key in [VK_RETURN,VK_HOME,VK_END]) and
          UpdateSettings(EditorRow,SpinEdit1.Text,SpinEdit1) then
          begin StringGrid1.SetFocus;
                if Key<>VK_RETURN then
                   PostMessage(StringGrid1.Handle, WM_KEYDOWN, Key, 0);
                Key:=0;
          end
  else if Key=VK_ESCAPE then
          begin SpinEdit1.Value:=StrToInt(StringGrid1.Cells[0,EditorRow]);
                SpinEdit1.SelLength:=Length(SpinEdit1.Text);
                EscapeEnabled:=False;
                UpdateSettings(EditorRow,SpinEdit1.Text,SpinEdit1);
          end;
end;

procedure TOptionsForm.EditorExit(Sender: TObject);
begin
  Edit1.CharCase:=ecNormal;
  CloseEditors(False);
end;

procedure TOptionsForm.FormDeactivate(Sender: TObject);
begin
  CloseEditors(False);
end;

function  TOptionsForm.RowToSettings(Row:Integer):TSettings;
begin
  Result:=TSettings(Integer(StringGrid1.Objects[0,Row]));
end;

procedure TOptionsForm.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ActiveControl=SpinEdit1 then with SpinEdit1 do begin
     Value:=Value-Increment;
     Handled:=True;
     end
  else if ActiveControl=Edit1 then begin
          Handled:=True;
          EditorExit(Edit1);
          ActiveControl:=StringGrid1;
          if StringGrid1.Row<Pred(StringGrid1.RowCount) then
             StringGrid1.Row:=Succ(StringGrid1.Row);
          end;
end;

procedure TOptionsForm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ActiveControl=SpinEdit1 then with SpinEdit1 do begin
     Value:=Value+Increment;
     Handled:=True;
     end
  else if ActiveControl=Edit1 then begin
          Handled:=True;
          EditorExit(Edit1);
          ActiveControl:=StringGrid1;
          if StringGrid1.Row>0 then
             StringGrid1.Row:=Pred(StringGrid1.Row);
          end;
end;

procedure TOptionsForm.Edit1Change(Sender: TObject);
var Editor:TSettingsEditor;
begin
  Editor:=SettingsEditor[RowToSettings(EditorRow)];
  if   Editor=seColor then begin
       SetEditColor;
       end
  else if SelectBtn.Visible and
          //Panel1.Visible and
          (not (Editor in [seColor,seSoundFileName,sePlayListItem,seImageFileName,seFilePath]))
          then with StringGrid1 do
            begin Edit1.Text:=Cells[0,EditorRow];
                  Edit1.SelLength:=Length(Edit1.Text);
            end;
end;

procedure TOptionsForm.MenuItemOpenFileClick(Sender: TObject);
begin
  if CloseEditors(False)
     and
     (((Sender= GeneratorForm) and GeneratorForm.CommitOrDropChanges)
      or
      ((Sender<>GeneratorForm) and CommitOrDropChanges)
     )
     then with OpenDialog1 do begin
     FileName:='';
     if Sender=GeneratorForm then begin
        FilterIndex:=GeneratorForm.FilterIndex;
        InitialDir:=GeneratorForm.SettingsDirectory;
        Title:=MainForm.Generator.GeneratorName;
        end
     else begin
        FilterIndex:=Self.SettingsFilterIndex;
        InitialDir:=Self.SettingsDirectory;
        Title:=Application.Title;
        end;
     Title:=Title+SUB_TITLE_SEPARATOR+SettingsText+SUB_TITLE_SEPARATOR+OpenSetCaptionText;

     if Execute and (FileName<>'') then begin
        Self.Update; // to repaint the form after the dialog has gone
        if Sender=GeneratorForm then begin
           GeneratorForm.FilterIndex:=FilterIndex;
           GeneratorForm.SettingsDirectory:=ExtractFilePath(FileName);
           end
        else begin
           OpenDialog1.InitialDir  :=ExtractFilePath(FileName);
           OpenDialog2.InitialDir  :=InitialDir;
           SaveDialog1.InitialDir  :=InitialDir;
           OpenDialog2.FilterIndex :=FilterIndex;
           SaveDialog1.FilterIndex :=FilterIndex;
           SaveDialog1.FileName    :=FileName;
           Self.SettingsFilterIndex:=FilterIndex;
           Self.SettingsDirectory  :=OpenDialog1.InitialDir;
           end;

        if   IsASettingsFile(FileName) then begin
             if Sender=GeneratorForm then
                GeneratorForm.LoadSettingsFromFile(FileName)
             else
                if   LoadFromFile(FileName,True) then begin
                     if AddItemOrMoveItemToTopOfComboBox(OpenForm.SkinsComboBox,SKINS_CAPACITY,FileName,False)=0 then // '0': new item
                        MainForm.Skins.Skins[0,SKIN_SCRIPT_INDEX]:=MainForm.Skins.SettingsScriptFileName;
                     MainForm.Skins.LastSkinNo:=0;
                     MainForm.Skins.UpdateMenu;
                     Modified:=True; ActiveControl:=OKBtn;
                     ModalResult:=mrOk; Close; ModalResult:=mrOk;
                     end
                else LoadData // reset the settings; the import may have left some garbage
                end
        else Error(PChar(Format(NotASettingsFileText__,[FileName])),Title);
        end;
    end;
end;

procedure TOptionsForm.MenuItemSaveAsFileClick(Sender: TObject);
var OrgName,FileNamePrefix:String;
begin
  if CloseEditors(False) and
     OpenForm.CloseText(tTextFile) and
     OpenForm.CloseText(tSkinScript) then with SaveDialog1 do begin
     if Sender=GeneratorForm then begin
        FilterIndex:=GeneratorForm.FilterIndex;
        InitialDir:=GeneratorForm.SettingsDirectory;
        FileNamePrefix:=MainForm.Generator.GeneratorName;
        end
     else begin
        FilterIndex:=Self.SettingsFilterIndex;
        InitialDir:=Self.SettingsDirectory;
        FileNamePrefix:=Application.Title;
        end;
     Title:=FileNamePrefix+SUB_TITLE_SEPARATOR+SettingsText+SUB_TITLE_SEPARATOR+SaveSettingsCaptionText;

     FileName:=MakeNewFileName(StrWithTrailingPathDelimiter(InitialDir)+FileNamePrefix+SPACE+SettingsText,SETTINGS_FILE_EXT,True);
     OrgName   :=FileName;
     InitialDir:=ExtractFilePath(FileName);
     FileName  :=ExtractFileName(FileName);

     if Execute then begin
        Self.Update; // to repaint the form after the dialog has gone

        if StrEqual(StrWithoutTrailingPathDelimiter(ExtractFilePath(FileName)),OptionsForm.SettingsPath) and
           StrEqual(ExtractFileNameWithoutPathAndExtension(FileName),TEXT_APPLICATION_TITLE_SHORT) then
           Msg(Format(ReservedNameText__+NL+NL+PleaseChangeItAndTryAgainText,[ExtractFileNameWithoutPathAndExtension(FileName)]),Caption,MB_OK+MB_ICONSTOP)
        else begin
           if Sender=GeneratorForm then begin
              GeneratorForm.FilterIndex:=FilterIndex;
              GeneratorForm.SettingsDirectory:=ExtractFilePath(FileName);
              end
           else begin
              SaveDialog1.InitialDir  :=ExtractFilePath(FileName);
              OpenDialog1.InitialDir  :=InitialDir;
              OpenDialog2.InitialDir  :=InitialDir;
              OpenDialog1.FileName    :=FileName;
              OpenDialog1.FilterIndex :=FilterIndex;
              OpenDialog2.FilterIndex :=FilterIndex;
              Self.SettingsFilterIndex:=FilterIndex;
              Self.SettingsDirectory  :=OpenDialog1.InitialDir;
              end;

           if ExtractFileExt(FileName)='' then
              FileName:=ChangeFileExt(FileName,SETTINGS_FILE_EXT);

           if (AnsiCompareText(OrgName,FileName)<>0)
              and
              FileExists(FileName)
              and
              (Msg(Format(FileExistsText__,[FileName])+NL+NL+OverwriteItText,
                   Title,
                   MB_YESNO+MB_ICONQUESTION+MB_DEFBUTTON2)
               <> IDYES) then begin
               end
           else
              if Sender=GeneratorForm then
                 GeneratorForm.SaveSettingsToFile(FileName)
              else
                 if SaveToFile(FileName,False) then
                    with MainForm.Skins do with OpenForm.SkinsComboBox do begin
                      LastSkinNo:=AddItemOrMoveItemToTopOfComboBox(OpenForm.SkinsComboBox,SKINS_CAPACITY,FileName,False);
                      if   (Items.Count<>0) and
                           (AnsiCompareText(Skins[0,SKIN_NAME_INDEX],FileName)=0) then
                           try    Skins[0,SKIN_SCRIPT_INDEX]:=SettingsScriptFileName;
                                  if   UpdateMenu then //
                                  else Error(TEXT_TASK_FAILED,'');
                           except on E:Exception do Error(E.Message,'');
                           end
                      else Error(TEXT_TASK_FAILED,'');
                      end;
           end;
        end;
     end;
end;

procedure TOptionsForm.MenuItemDeleteFilesClick(Sender: TObject);
var Result:Boolean;
begin
  if CloseEditors(False) then with OpenDialog2 do begin
     if   Self.SettingsFilterIndex=2 then
          FilterIndex:=3 // the delete dialog has one more filter (*.sok) than the load/save settings dialogs
     else FilterIndex:=Self.SettingsFilterIndex;
     InitialDir:=Self.SettingsDirectory;

     Result:=DeleteFilesDialog(OpenDialog2,Self,StatusBar1.Panels[1]);

     if Result then begin
        OpenDialog2.InitialDir  :=ExtractFilePath(Files[0]);
        OpenDialog1.InitialDir  :=InitialDir;
        SaveDialog1.InitialDir  :=InitialDir;
        SaveDialog1.FilterIndex :=FilterIndex;
        OpenDialog1.FilterIndex :=FilterIndex;
        Self.SettingsFilterIndex:=Min(2,FilterIndex);
        Self.SettingsDirectory  :=OpenDialog1.InitialDir;
        end;
     end;
end;

procedure TOptionsForm.LoadDefaultValues;
var s:TSettings;
begin
  for s:=Low(s) to High(s) do
      if SettingsEditor[s]<>seNone then
         fSettingsString[s]:=DefaultSettingsString[s];
end;

(* a newer version of 'LoadFromFile' (see below) uses the faster 'Skins.LoadFromFile'

function  TOptionsForm.LoadFromFile(const FileName:String):Boolean;
var {i:TSettings;} oCursor:TCursor; {SL:TStringList;} IniFile:TIniFile;

  procedure TreeWalk(Node:TTreeNode; const SectionName:String);
  var N:TTreeNode;
  begin
    if Node<>nil then begin
      N:=Node;
      while N<>nil do begin
        if not N.HasChildren then
           SettingsString[TSettings(N.AbsoluteIndex)]:=IniFile.ReadString(SectionName,N.Text,SettingsString[TSettings(N.AbsoluteIndex)]);
        N:=N.GetNextSibling;
        end;

      N:=Node;
      while N<>nil do begin
        if N.HasChildren then begin
           if SettingsEditor[TSettings(N.AbsoluteIndex)]<>seNone then
              SettingsString[TSettings(N.AbsoluteIndex)]:=IniFile.ReadString(FullItemName(N),N.Text,SettingsString[TSettings(N.AbsoluteIndex)]);
           TreeWalk(N.GetFirstChild,FullItemName(N));
           end;
        N:=N.GetNextSibling;
        end;
      end;
  end;

begin  // LoadFromFile
  Result:=True;
  try
         if FileExists(FileName) then begin
{           // quick version, used in test phase
            SL:=TStringList.Create;
            try
              SL.LoadFromFile(FileName);
              for i:=Low(i) to High(i) do
                  if Ord(i)<SL.Count then
                     FSettingsString[i]:=Trim(SL[Ord(i)]);
            finally
              SL.Free;
            end;
            end;
}
            IniFile:=TIniFile.Create(FileName);
            oCursor:=Screen.Cursor;
            try     Screen.Cursor:=crHourGlass;
                    if TreeView1.Items.Count>0 then
                       TreeWalk(TreeView1.Items[0],'');
            finally Screen.Cursor:=oCursor; IniFile.Free;
            end;

            end
         else raise Exception.Create(Format(FileNotFoundText__,[FileName]));
         TreeToGrid; ShowStatus;
         //raise Exception.Create('Test # 4332754');
  except on E:Exception do
            Result:=Error(Format(OpenFileFailedShortText__+NL+NL+
                          TEXT_ERROR_REPORT+NL+E.Message,[FileName]),
                          OpenDialog1.Title);
  end;
  TreeToGrid;
  ShowStatus;
end;
*)

function  TOptionsForm.LoadFromFile(const FileName:String; LoadDefaultValues__:Boolean):Boolean;
var t:TTextType;
begin
  Result:=True; Update;
  try    try     if FileExists(FileName) then with OpenForm do begin
                    for t:=Low(t) to High(t) do ClearText(t); // ensure that the file is loaded from disk and not from the program's cache
                    if   LoadTextFromFile(FileName,tTextFile) then begin
                         if   LoadDefaultValues__ then // 'True': set default values before loading the new settings; otherwise, the loaded values are treated as supplementary to existing settings
                              LoadDefaultValues;
                         if   MainForm.Skins.Parse(tTextFile,FileName,True) then // ok
                         else raise Exception.Create(TEXT_TASK_FAILED);
                         end
                    else raise Exception.Create(TEXT_TASK_FAILED);
                    end
                 else raise Exception.Create(Format(FileNotFoundText__,[FileName]));
         finally TreeToGrid;
                 ShowStatus;
         end;
  except on E:Exception do
            Result:=Error(Format(OpenFileFailedShortText__+NL+NL+
                          TEXT_FAILURE_DESCRIPTION+NL+E.Message,[FileName]),
                          OpenDialog1.Title);
  end;
end;

(*
function TOptionsForm.SaveToFile(const FileName:String):Boolean;
var {i:TSettings;} oCursor:TCursor; SL:TStringList;

  procedure TreeWalk(Node:TTreeNode);
  var N:TTreeNode;
  begin
    if Node<>nil then begin
      N:=Node;
      while N<>nil do begin
        if not N.HasChildren then
           SL.Add(N.Text+EQUAL+SettingsString[TSettings(N.AbsoluteIndex)]);
        N:=N.GetNextSibling;
        end;

      N:=Node;
      while N<>nil do begin
        if N.HasChildren then begin
           SL.Add('');
           SL.Add(LEFT_BRACKET+FullItemName(N)+RIGHT_BRACKET);
           if SettingsEditor[TSettings(N.AbsoluteIndex)]<>seNone then
              SL.Add(N.Text+EQUAL+SettingsString[TSettings(N.AbsoluteIndex)]);
           TreeWalk(N.GetFirstChild);
           end;
        N:=N.GetNextSibling;
        end;
      end;
  end;

begin // SaveToFile
  Result:=True; oCursor:=Screen.Cursor;
  try    SL:=TStringList.Create;
         try     Screen.Cursor:=crHourGlass;

                 // quick version, used in test phase
                 //for i:=Low(i) to High(i) do SL.Add(SettingsString[i]);

                 SL.Add(LEFT_BRACKET+SettingsFileID+RIGHT_BRACKET);
                 if TreeView1.Items.Count>0 then
                    TreeWalk(TreeView1.Items[0]);

                 SL.SaveToFile(FileName);
         finally Screen.Cursor:=oCursor; SL.Free;
         end;
         //raise Exception.Create('Test # 433335');
  except on E:Exception do
            Result:=Error(Format(SaveFileFailedText__+NL+NL+
                          TEXT_ERROR_REPORT+NL+E.Message,[FileName]),
                          SaveDialog1.Title);
  end;
end;
*)

function TOptionsForm.SaveToFile(const FileName:String; AllItems__:Boolean):Boolean;
var s1,s2:String; PictureType:GView_.TPictureType; Setting:TSettings;
    oCursor:TCursor;
    Visited:array[TSettings] of Boolean;
    Node,Next:TTreeNode; SL:TStringList;

  procedure AddSectionHeader(Node__:TTreeNode);
  begin
    if (Node__<>nil) and (not Visited[TSettings(Node__.AbsoluteIndex)]) then
       with Node__ do begin
         //AddSectionHeader(Parent);
         SL.Add('');
         SL.Add(LEFT_BRACKET+FullItemName(Node__)+RIGHT_BRACKET);
         Visited[TSettings(Node__.AbsoluteIndex)]:=True;
         end;
  end;

begin // SaveToFile
  Result:=True; oCursor:=Screen.Cursor;
  try    SL:=TStringList.Create;
         try     Screen.Cursor:=crHourGlass;

                 SL.Add(LEFT_BRACKET+SettingsFileID+RIGHT_BRACKET);
                 if not AllItems__ then SL.Add(LEFT_PAREN+OnlyNonDefaultValuesText+RIGHT_PAREN);

                 if TreeView1.Items.Count=Succ(Ord(High(Setting))-Ord(Low(Setting))) then begin
                    FillChar(Visited,SizeOf(Visited),0);
                    for Setting:=Low(Setting) to High(Setting) do begin
                        Node:=TreeView1.Items[Ord(Setting)];
                        with Node do begin
                          if (not HasChildren)
                             and
                             (AllItems__ or
                              (SettingsString[Setting]<>DefaultSettingsString[Setting])
                             )
                             and
                             (SettingsEditor[Setting]<>seNone)
                             then begin
                             s1:=SettingsString[Setting];
                             if IsAFileNameSetting(Node) and
                                IsAPictureSetting(Node.AbsoluteIndex,PictureType) and
                                StrEqual(StrWithoutTrailingPathDelimiter(ExtractFilePath(s1)),
                                         StrWithoutTrailingPathDelimiter(SettingsPath))
                                then begin
                                // automatically created skin-components must be renamed;
                                // otherwise they'll be overwritten when the user loads
                                // another skin
                                s2:=StrWithTrailingPathDelimiter(ExtractFilePath(FileName))+
                                   ExtractFileNameWithoutPathAndExtension(FileName)+
                                    SUB_TITLE_SEPARATOR+
                                    GView_.PICTURE_TYPE_NAME[PictureType]+
                                    ExtractFileExt(s1);
                                if FileExists(s1) and (not StrEqual(s1,s2)) then begin
                                   SysUtils.DeleteFile(s2); RemoveDir(s2);
                                   if   Windows.CopyFile(PChar(s1),PChar(s2),True) then begin
                                        s1:=s2;
                                        // don't change the settings; the old file
                                        // is intact, and changing settings would
                                        // bind the current settings to the saved skin;
                                        // if would also confuse the users, since they
                                        // often haven't modified anything at this time
                                        //
                                        // UpdateSettings(-AbsoluteIndex,s1,nil);
                                        //
                                        end
                                   else raise Exception.Create(Format(FileCopyFailedText__,[s1,s2]));
                                   end
                                else s1:=s2;

                                end;
                             AddSectionHeader(Parent);
                             SL.Add(Text+EQUAL+s1);
                             end;
                          if Setting<High(Setting) then begin
                             Next:=TreeView1.Items[Succ(Ord(Setting))];
                             if (Next.Level<Node.Level) and (Next.Parent<>nil) then
                                // stepping one level back;
                                // clear 'Visited' for the parent of the next node
                                // in order to trigger a section header for the next node
                                Visited[TSettings(Next.Parent.AbsoluteIndex)]:=False;
                             end;
                          end;
                        end;
                    end
                 else raise Exception.Create(TEXT_TASK_FAILED); // shouldn't happen

                 SL.SaveToFile(FileName);
         finally Screen.Cursor:=oCursor; SL.Free;
         end;
  except on E:Exception do
            Result:=Error(Format(SaveFileFailedText__+NL+NL+
                          TEXT_FAILURE_DESCRIPTION+NL+E.Message,[FileName]),
                          SaveDialog1.Title);
  end;
end;

function  TOptionsForm.SettingsFileID:String;
begin
  Result:=TEXT_APPLICATION_TITLE_LONG+SUB_TITLE_SEPARATOR+SettingsText;
end;

function  TOptionsForm.SettingsPath:String; // path without trailing path-delimiter
begin
  Result:=StrWithTrailingPathDelimiter(SkinsPath)+TEXT_APPLICATION_TITLE_SHORT+SPACE+SettingsText;
end;

function  TOptionsForm.IsASettingsFile(const FileName:String):Boolean;
begin
  Result:=FileStartsWith(FileName,LEFT_BRACKET+SettingsFileID+RIGHT_BRACKET)
          or
          FileStartsWith(FileName,LEFT_BRACKET+Caption+RIGHT_BRACKET); // [<caption>]: version <= 1.128
end;

procedure TOptionsForm.SetEditColor;
var i:Integer;
begin
  with Edit1 do
    if   (SettingsEditor[RowToSettings(EditorRow)]=seColor) and
         SafeStrToInt(RGB_BGR(Copy(Edit1.Text,1,6)),True,i) then
         begin Edit1.Color:=TColor(i);
               Edit1.Font.Color:=ContrastColor(Edit1.Color);
         end
    else if ReadOnly then
         begin Edit1.Color:=clBtnFace;
               Edit1.Font.Color:=clBtnText;
         end
    else begin Edit1.Color:=clWhite;
               Edit1.Font.Color:=clBlack;
         end
end;

function TOptionsForm.InternalToExternalFileTypeFilter(const s:String):String;
var i:Integer;
begin
  Result:=s; i:=AnsiPos('*.',Result);
  while i<>0 do begin Delete(Result,i,2); i:=AnsiPos('*.',Result); end;
  i:=AnsiPos(';',Result);
  while i<>0 do begin Result[i]:=','; i:=AnsiPos(';',Result); end;
  for i:=Length(Result) downto 1 do
      if Result[i]=',' then Insert(' ',Result,i+1);
end;

function TOptionsForm.ExternalToInternalFileTypeFilter(const s:String):String;
var i:Integer;
begin
  Result:=StrRemoveChar(StrRemoveChar(StrRemoveChar(StrRemoveChar(s,SPACE),PERIOD),STAR),BAR);
  i:=AnsiPos(',',Result);
  while i<>0 do begin Result[i]:=';'; i:=AnsiPos(',',Result); end;
  if Result<>'' then begin
     Insert('*.',Result,1);
     if Result[Length(Result)]=';' then Delete(Result,Length(Result),1);
     for i:=Length(Result) downto 1 do
         if Result[i]=';' then Insert('*.',Result,i+1);
     end;
end;

procedure TOptionsForm.SetToDefaultValue(Node:TTreeNode);
var i:Integer;
begin
  if Node<>nil then with TreeView1 do begin
     i:=Node.AbsoluteIndex;
     if (SettingsEditor[TSettings(i)]=seImageFileName) and
        (i+5<TreeView1.Items.Count) and {reset image-section as well as filename}
        (AnsiCompareText(ImageSectionText,TreeView1.Items[Succ(i)].Text)=0) then begin
        SetToDefaultValue(TreeView1.Items[Succ(i)]);
        end;

     // set default value for the node and all its descendants
     repeat if   SettingsEditor[TSettings(i)]<>seNone then
                 UpdateSettings(-i,DefaultSettingsString[TSettings(i)],nil);
            Inc(i);
     until  (i>=Items.Count) or (Items[i].Level<=Node.Level);
     end;
end;

procedure TOptionsForm.MenuItemViewClick(Sender: TObject);
var i:Integer;
begin
  with TreeView1 do begin
    MenuItemCollapseTree.Enabled:=False;
    MenuItemExpandTree  .Enabled:=False;
    for i:=0 to Pred(Items.Count) do
        if Items[i].HasChildren and Items[i].Expanded then begin
           MenuItemCollapseTree.Enabled:=True; break;
           end;
    for i:=0 to Pred(Items.Count) do
        if Items[i].HasChildren and (not Items[i].Expanded) then begin
           MenuItemExpandTree.Enabled:=True; break;
           end;
    end;
end;

procedure TOptionsForm.MenuItemCollapseTreeClick(Sender: TObject);
var i:Integer; oCursor:TCursor;
begin
  oCursor:=Screen.Cursor;
  if not IsBusy then with TreeView1 do
     try     IsBusy:=True; Screen.Cursor:=crHourGlass;
             Items.BeginUpdate;
             //FullCollapse; // doesn't work; reason unknown; instead do it  manually ...
             for i:=Pred(Items.Count) downto 0 do
                 if Items[i].Expanded then
                    Items[i].Expanded:=False;
             Items.EndUpdate;
             Selected:=Items[0]; Selected:=Items[0];
     finally IsBusy:=False; Screen.Cursor:=oCursor;
             TreeToGrid;
             TreeView1Change(TreeView1,Selected);
             TreeView1.SetFocus;
     end;
end;

procedure TOptionsForm.MenuItemExpandTreeClick(Sender: TObject);
var oCursor:TCursor; oSelected:TTreeNode;
begin
  oCursor:=Screen.Cursor; oSelected:=TreeView1.Selected;
  if not IsBusy then with TreeView1 do
     try     IsBusy:=True; Screen.Cursor:=crHourGlass;
             Items.BeginUpdate;
             FullExpand;
             Items.EndUpdate;
     finally IsBusy:=False; Screen.Cursor:=oCursor;
             Selected:=oSelected; Selected:=oSelected;
             TreeToGrid;
             TreeView1Change(TreeView1,Selected);
             TreeView1.SetFocus;
     end;
end;

procedure TOptionsForm.MenuItemSkinsClick(Sender: TObject);
begin
  //MenuItemRecentSkins.Visible:=MenuItemRecentSkins.Count>2; // '-' and 'Clear recent skins' are static members of the sub-menu
  //MenuItemRecentSkinsSeparator.Visible:=MenuItemRecentSkins.Visible;
  MenuItemRecentSkins.Enabled:=MenuItemRecentSkins.Count>2; // '-' and 'Clear recent skins' are static members of the sub-menu
end;

procedure TOptionsForm.MenuItemLoadSkinFromHistoryClick(Sender: TObject);
var Index:Integer; OriginalResetSoundEffectsOnLoadingASkin:Boolean;
begin //
  if CloseEditors(False) then begin
     Update;
     if Sender is TMenuItem then with Sender as TMenuItem do Index:=Tag
     else Index:=-1;
     if (Index>=0) and (Index<OpenForm.SkinsComboBox.Items.Count) then
        with MainForm.Skins do begin
          OriginalResetSoundEffectsOnLoadingASkin:=MainForm.Sound.ResetSoundEffectsOnLoadingASkin;
          try
            MainForm.Sound.ResetSoundEffectsOnLoadingASkin:=StrToBool(SettingsString[stSoundResetSoundEffectsOnLoadingASkin]);
            OpenForm.CurrentFileName:='';
            if LoadFromFile(Skins[Index,SKIN_NAME_INDEX],
                            Skins[Index,SKIN_SCRIPT_INDEX]) then begin
               Modified:=True; ActiveControl:=OKBtn;
               ModalResult:=mrOk; Close; ModalResult:=mrOk;
               end;
          finally
            MainForm.Sound.ResetSoundEffectsOnLoadingASkin:=OriginalResetSoundEffectsOnLoadingASkin;
            TreeToGrid; ShowStatus;
          end;
          end;
     end;
end;

procedure TOptionsForm.MenuItemClearRecentSkinsClick__(Sender: TObject);
begin
  MainForm.Skins.ClearSkins;
end;

procedure TOptionsForm.MenuItemLoadSkinClick(Sender: TObject);
var i:Integer; Result:Boolean; s,FileName,ScriptFileName,Filter:String;
    CloseAction:TCloseAction; oOnMessage:TMessageEvent;
begin
  ModalResult:=mrNone;

  if CloseEditors(False) and CommitOrDropChanges then begin

     if (Sender=MainForm.BtnOpenNext) or (not Assigned(Sender)) then begin
        //Result:=MainForm.Skins.LoadNextSkinFromHistory;
        Result:=MainForm.Skins.LoadNextOrPreviousSkinFromFolder(+1);
        fSettingsString[stSoundVolume]:=IntToStr(((MainForm.Music.Volume64Kibi*100)+32767) div 65535);
        end
     else if Sender=MainForm.BtnOpenPrior then begin
             //Result:=MainForm.Skins.LoadNextSkinFromHistory;
             Result:=MainForm.Skins.LoadNextOrPreviousSkinFromFolder(-1);
             fSettingsString[stSoundVolume]:=IntToStr(((MainForm.Music.Volume64Kibi*100)+32767) div 65535);
             end
          else begin
             Result:=False; FileName:=''; ScriptFileName:=''; Filter:=IMAGE_FILES_FILTER;
             with OpenForm.SkinsComboBox do
               if Items.Count<>0 then begin
                  s:=MainForm.Skins.SettingsScriptFileName;
                  for i:=0 to Pred(Items.Count) do begin
                      FileName:=MainForm.Skins.Skins[i,SKIN_NAME_INDEX];
                      ScriptFileName:=MainForm.Skins.Skins[i,SKIN_SCRIPT_INDEX];
                      if   not StrEqual(ScriptFileName,s) then
                           break
                      else FileName:='';
                      end;

                  if FileExists(ScriptFileName) then begin
                     MainForm.Skins.ScriptFileName:=ScriptFileName;
                     if MainForm.Skins.ScriptFileName=ScriptFileName then
                        Filter:=MainForm.Skins.FileFilter;
                     end;
                  end
               else with OpenForm.SkinScriptsComboBox do
                      if Items.Count<>0 then begin
                         if   ItemIndex<0 then
                              ScriptFileName:=MainForm.Skins.Scripts[0        ,SKIN_SCRIPT_INDEX]
                         else ScriptFileName:=MainForm.Skins.Scripts[ItemIndex,SKIN_SCRIPT_INDEX];
                         if FileExists(ScriptFileName) then begin
                            MainForm.Skins.ScriptFileName:=ScriptFileName;
                            if MainForm.Skins.ScriptFileName=ScriptFileName then
                               Filter:=MainForm.Skins.FileFilter;
                            end;
                         end;

             if (FileName='') or (not FileExists(FileName)) then begin
                FileName:='';
                OpenForm.DirectoryListBox1.Directory:=StrWithoutTrailingPathDelimiter(MainForm.ApplicationDataPath);
                ScriptFileName:=MainForm.Skins.CommonSkinsScriptFileName;
                if FileExists(ScriptFileName) then begin
                   MainForm.Skins.ScriptFileName:=ScriptFileName;
                   if MainForm.Skins.ScriptFileName=ScriptFileName then
                      Filter:=MainForm.Skins.FileFilter;
                   end;
                end;

             if OpenForm.InitTask(otImage,osSkin,SkinsText,SkinText,
                                  {filename:} FileName,
                                  {filter:} Filter,
                                  {default filename:} '',
                                  Rect(0,0,0,0),clBlack,True,False,Rect(0,0,0,0)) then begin
                oOnMessage:=Application.OnMessage;
                try     if OpenForm.ShowModal=mrOk then with MainForm.Skins do
                           if (OpenForm.SkinScriptsComboBox.ItemIndex>=0) and
                              (Scripts[OpenForm.SkinScriptsComboBox.ItemIndex,SKIN_SCRIPT_INDEX]<>'') then begin
                              Update;
                              Result:=LoadFromFile(
                                OpenForm.CurrentFileName,
                                Scripts[OpenForm.SkinscriptsComboBox.ItemIndex,SKIN_SCRIPT_INDEX]);
                              if not Result then LoadData; // reset the settings; the import may have left some garbage
                              end;
                finally Application.OnMessage:=oOnMessage;
                        Application.OnHint:=ShowHint;
                        TreeToGrid; ShowStatus;
                end;
                end;
             end;

     if Result then begin
        Modified:=True; ActiveControl:=OKBtn;
        ModalResult:=mrOk;
        if   Screen.ActiveForm=Self then Close
        else FormClose(Self,CloseAction);
        ModalResult:=mrOk;

{
        if (MainForm.GamePictures.WallType=wtSeamlessWallNoTop) and
           (OpenForm.CurrentImageFileName<>'') and
           (OpenForm.Image1.Picture.BitMap<>nil) then
           with OpenForm.Image1.Picture.BitMap do begin
             s:=MakeNewFileName(OpenForm.CurrentImageFileName,BMP_FILE_EXT);
             if   s<>'' then
                  MainForm.Skins.Convert4x4WallSkinTo3x2WallSkin(OpenForm.Image1.Picture.BitMap,s)
             else Error(DiskFullText,TEXT_APPLICATION_TITLE);
             end;
}
        end;
     end;
end;

procedure TOptionsForm.LoadDefaultSkin(DefaultSkinWallType__:TDefaultSkinWallType; ResetSoundEffectsOnLoadingASkin__:Boolean);
var oCursor:TCursor; p:TPictureType;
begin
  oCursor:=Screen.Cursor;
  if CloseEditors(False) then with TreeView1 do
     try
       Screen.Cursor:=crHourGlass;
       SetToDefaultValue(Items[Ord(stSmoothMoveAnimationEnabled)]);
       SetToDefaultValue(Items[Ord(stPlayerDirectionAnimationEnabled)]);
       SetToDefaultValue(Items[Ord(stMoveAnimationForkliftDrivingEnabled)]);
       SetToDefaultValue(Items[Ord(stPathFindingShowLegalMovesPlayerCursorEnabled)]);
       SetToDefaultValue(Items[Ord(stPathFindingShowLegalMovesBoxCursorEnabled)]);
       SetToDefaultValue(Items[Ord(stPathFindingShowLegalMovesAnimation)]);
       SetToDefaultValue(Items[Ord(stSolutionsAnimation)]);
       if ResetSoundEffectsOnLoadingASkin__ then
          SetToDefaultValue(Items[Ord(stSoundEffects)]);


//     resetting the background is a dilemma;
//     it's a good idea if skins have their own background;
//     if they haven't, resetting is a nuisance because
//     the user's personal background is discarded;
//     therefore, the best choice is probably to leave the
//     the background as it is;
//
//     SetToDefaultValue(Items[Ord(stGraphicsBackground)]);

       SetToDefaultValue(Items[Ord(stGraphicsBoard)]);
       if   DefaultSkinWallType__<>dsTiledWalls then
            fSettingsString[stGraphicsBoardFiguresWallType]:=WallTypeText[wtSeamlessWallWithCap]
       else fSettingsString[stGraphicsBoardFiguresWallType]:=WallTypeText[wtTiledWall];

       if   DefaultSkinWallType__=dsBrickWalls then begin
            FSettingsString[stGraphicsBoardFiguresWallImageSectionLeft ]:=IntToStr(DEFAULT_SKIN_BRICK_WALL_LEFT);
            FSettingsString[stGraphicsBoardFiguresWallImageSectionTop  ]:=IntToStr(DEFAULT_SKIN_BRICK_WALL_TOP );
            FSettingsString[stGraphicsBoardFiguresWallImageAntiAliasing]:=AntiAliasingText[aaFilter];
            end;

       for  p:=ptPlayer to ptBoxOnGoalAnimation do
            if not (p in [ptGoal,ptWall,ptFloor]) then
               FSettingsString[TSettings(PictureSettingsOffset[p]+10)]:=IntToStr(DEFAULT_SKIN_MASK_BITMAP_PCT);

       fSettingsString[stSoundVolume]:=IntToStr(((MainForm.Music.Volume64Kibi*100)+32767) div 65535);
     finally
       Screen.Cursor:=oCursor;
     end;
end;

procedure TOptionsForm.MenuItemDefaultSkinClick(Sender: TObject);
begin //
  if CloseEditors(False) and
     (TreeView1.Items.Count=Succ(Ord(High(TSettings)))) then with TreeView1 do begin // sanity check, just to be sure that all items have been created
     Update;
     if        MenuItemDefaultSkinSeamlessWalls.Checked then LoadDefaultSkin(dsSeamlessWalls,StrToBool(SettingsString[stSoundResetSoundEffectsOnLoadingASkin]))
     else if   MenuItemDefaultSkinTiledWalls   .Checked then LoadDefaultSkin(dsTiledWalls   ,StrToBool(SettingsString[stSoundResetSoundEffectsOnLoadingASkin]))
          else                                               LoadDefaultSkin(dsBrickWalls   ,StrToBool(SettingsString[stSoundResetSoundEffectsOnLoadingASkin]));
     MainForm.Skins.LastSkinNo:=-1;
     if      Assigned( Sender ) and ( Sender is TMenuItem ) then
             MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified := FILE_NAME_PATH_DELIMITER + DefaultSkinTitleText + SUB_TITLE_Separator + StrRemoveChar( TMenuItem( Sender ).Caption, AMPERSAND );

     if      Sender=MenuItemDefaultSkinRedBoxes then begin
             FSettingsString[stPathFindingShowLegalMovesAnimationPlayerAnimationEnabled]:=BooleanText[True];
             FSettingsString[stSolutionsAnimationPlayerAnimationEnabled                ]:=BooleanText[True];
             FSettingsString[stPathFindingShowLegalMovesAnimationBoxAnimationEnabled   ]:=BooleanText[True];
             FSettingsString[stSolutionsAnimationBoxAnimationEnabled                   ]:=BooleanText[True];
             end
     else if Sender=MenuItemDefaultSkinYellowBoxes then begin
             FSettingsString[stGraphicsBoardFiguresBoxImageSectionLeft                 ]:=IntToStr(DEFAULT_SKIN_YELLOW_BOX_LEFT);
             FSettingsString[stGraphicsBoardFiguresBoxImageSectionTop                  ]:=IntToStr(DEFAULT_SKIN_YELLOW_BOX_TOP );
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionLeft           ]:=IntToStr(DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_LEFT);
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionTop            ]:=IntToStr(DEFAULT_SKIN_YELLOW_BOX_ON_GOAL_TOP );

             FSettingsString[stGraphicsBoardFiguresBoxAnimationImageSectionLeft        ]:=FSettingsString[stGraphicsBoardFiguresBoxImageSectionLeft      ];
             FSettingsString[stGraphicsBoardFiguresBoxAnimationImageSectionTop         ]:=FSettingsString[stGraphicsBoardFiguresBoxImageSectionTop       ];
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionLeft  ]:=FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionLeft];
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionTop   ]:=FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionTop ];

             FSettingsString[stPathFindingShowLegalMovesAnimationPlayerAnimationEnabled]:=BooleanText[True];
             FSettingsString[stSolutionsAnimationPlayerAnimationEnabled                ]:=BooleanText[True];
             FSettingsString[stPathFindingShowLegalMovesAnimationBoxAnimationEnabled   ]:=BooleanText[True];
             FSettingsString[stSolutionsAnimationBoxAnimationEnabled                   ]:=BooleanText[True];
             end
     else if Sender=MenuItemDefaultSkinYellowCrates then begin
             FSettingsString[stGraphicsBoardFiguresPlayerImageSectionLeft              ]:=IntToStr(DEFAULT_SKIN_VIOLET_PLAYER_LEFT);
             FSettingsString[stGraphicsBoardFiguresPlayerImageSectionTop               ]:=IntToStr(DEFAULT_SKIN_VIOLET_PLAYER_TOP );
             FSettingsString[stGraphicsBoardFiguresPlayerOnGoalImageSectionLeft        ]:=FSettingsString[stGraphicsBoardFiguresPlayerImageSectionLeft];
             FSettingsString[stGraphicsBoardFiguresPlayerOnGoalImageSectionTop         ]:=FSettingsString[stGraphicsBoardFiguresPlayerImageSectionTop ];

             FSettingsString[stGraphicsBoardFiguresBoxImageSectionLeft                 ]:=IntToStr(DEFAULT_SKIN_YELLOW_CRATE_LEFT);
             FSettingsString[stGraphicsBoardFiguresBoxImageSectionTop                  ]:=IntToStr(DEFAULT_SKIN_YELLOW_CRATE_TOP );
             FSettingsString[stGraphicsBoardFiguresBoxImageAntiAliasing                ]:=AntiAliasingText[aaFilter];
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionLeft           ]:=IntToStr(DEFAULT_SKIN_YELLOW_CRATE_ON_GOAL_LEFT);
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionTop            ]:=IntToStr(DEFAULT_SKIN_YELLOW_CRATE_ON_GOAL_TOP );
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageAntiAliasing          ]:=AntiAliasingText[aaFilter];

             FSettingsString[stGraphicsBoardFiguresBoxAnimationImageSectionLeft        ]:=FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionLeft];
             FSettingsString[stGraphicsBoardFiguresBoxAnimationImageSectionTop         ]:=FSettingsString[stGraphicsBoardFiguresBoxOnGoalImageSectionTop ];
             FSettingsString[stGraphicsBoardFiguresBoxAnimationImageBackgroundColorTolerancePct]
                                                                                        :=IntToStr(DEFAULT_SKIN_YELLOW_CRATE_MASK_BITMAP_PCT);
             FSettingsString[stGraphicsBoardFiguresBoxAnimationImageAntiAliasing       ]:=AntiAliasingText[aaFilter];
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionLeft  ]:=FSettingsString[stGraphicsBoardFiguresBoxImageSectionLeft];
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalAnimationImageSectionTop   ]:=FSettingsString[stGraphicsBoardFiguresBoxImageSectionTop ];
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalAnimationImageBackgroundColorTolerancePct]
                                                                                        :=IntToStr(DEFAULT_SKIN_YELLOW_CRATE_MASK_BITMAP_PCT);
             FSettingsString[stGraphicsBoardFiguresBoxOnGoalAnimationImageAntiAliasing ]:=AntiAliasingText[aaFilter];

             FSettingsString[stPathFindingShowLegalMovesAnimationBoxAnimationEnabled   ]:=BooleanText[True];
             FSettingsString[stSolutionsAnimationBoxAnimationEnabled                   ]:=BooleanText[True];
             end;

     if      MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked then begin
             SetToDefaultValue(Items[Ord(stGraphicsBackground)]);
             SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBoardBackground)]);
             MenuItemBuiltinButtonsClick(MenuItemBuiltinBlueButtons);
             end
     else    MenuItemBuiltinButtonsClick(nil);
     end;
end;

function  TOptionsForm.GetPictureTypeFileNameIndex(PictureType__:GView_.TPictureType):TSettings;
begin
    if   PictureType__<ptPlayer then
         Result:=TSettings(Ord(PictureSettingsOffset[PictureType__])+
                           Ord(stGraphicsBackgroundImageFileName)-
                           Ord(stGraphicsBackgroundImage))
    else Result:=TSettings(Ord(PictureSettingsOffset[PictureType__])+
                           Ord(stGraphicsBoardFiguresPlayerImageFileName)-
                           Ord(stGraphicsBoardFiguresPlayer));
end;

function  TOptionsForm.GetPictureTypeFileName(PictureType__:GView_.TPictureType):String;
begin
  Result:=SettingsString[PictureTypeFileNameIndex[PictureType__]];
end;

function  TOptionsForm.GetPictureTypeSection(PictureType__:GView_.TPictureType):TRect;
var s:TSettings;
begin // returns rect(left,top,right,bottom) even though end-users see sections as (left,top,width,height)
  with Result do begin
    FillChar(Result,SizeOf(Result),0);
    s:=TSettings(Ord(PictureTypeFileNameIndex[PictureType__])+2);
    SafeStrToInt(SettingsString[s],False,Left  ); Inc(s);
    SafeStrToInt(SettingsString[s],False,Top   ); Inc(s);
    SafeStrToInt(SettingsString[s],False,Right ); Inc(s); Inc(Right,Left);
    SafeStrToInt(SettingsString[s],False,Bottom);         Inc(Bottom,Top);
    end;
end;

procedure TOptionsForm.SetPictureTypeFileName(PictureType__:GView_.TPictureType; const FileName__:String);
begin
  UpdateSettings(-Ord(PictureTypeFileNameIndex[PictureType__]),FileName__,nil);
end;

procedure TOptionsForm.SetPictureTypeSection(PictureType__:GView_.TPictureType; const Rect__:TRect);
var s:TSettings;
begin
  with Rect__ do begin
    s:=TSettings(Ord(PictureTypeFileNameIndex[PictureType__])+2);
    UpdateSettings(-Ord(s),IntToStr(Left      ),nil); Inc(s);
    UpdateSettings(-Ord(s),IntToStr(Top       ),nil); Inc(s);
    UpdateSettings(-Ord(s),IntToStr(Right-Left),nil); Inc(s); //sections are (left,top,height,width)
    UpdateSettings(-Ord(s),IntToStr(Bottom-Top),nil);
    end;
end;

procedure TOptionsForm.SetColorTheme( ColorTheme__ : TColorTheme );
begin // precondition: no input editors are active
  if //CloseEditors(False) and
     (TreeView1.Items.Count=Succ(Ord(High(TSettings)))) then with TreeView1 do // sanity check, just to be sure that all items have been created
     case ColorTheme__ of
       ctUndefined : begin
         end;
       ctBlue : begin // default
         SetToDefaultValue(Items[Ord(stGraphicsMenuTransparentButtons)]);
         SetToDefaultValue(Items[Ord(stGraphicsMenuTextShadow)]);
         SetToDefaultValue(Items[Ord(stGraphicsMenuButtons)]);
         SetToDefaultValue(Items[Ord(stGraphicsMenuButtonsEdges)]);
         SetToDefaultValue(Items[Ord(stGraphicsMenuButtonsFont)]);
         SetToDefaultValue(Items[Ord(stGraphicsStatus)]);
         SetToDefaultValue(Items[Ord(stGraphicsMenuPopupMenuColors)]);
         end;
       ctGreen : begin
         SetColorTheme( ctBlue );
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonImageSectionLeft),IntToStr(132),nil);
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonImageSectionTop),IntToStr(1),nil);
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(0,96,0),6)),nil);
         UpdateSettings(-Ord(stGraphicsMenuPopupMenuColorsButton),RGB_BGR(Misc_.IntToHex(RGB(0,128,0),6)),nil);

         UpdateSettings(-Ord(stGraphicsStatusImageSectionLeft),IntToStr(132),nil);
         UpdateSettings(-Ord(stGraphicsStatusImageSectionTop),IntToStr(1),nil);
         //UpdateSettings(-Ord(stGraphicsStatusPanelsTransparencyPct),IntToStr(70),nil);
         UpdateSettings(-Ord(stGraphicsStatusTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(0,96,0),6)),nil);
         UpdateSettings(-Ord(stGraphicsStatusSubTextsFontColor),RGB_BGR(Misc_.IntToHex(DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_GREEN,6)),nil);
         end;
       ctOrange : begin
         SetColorTheme( ctBlue );
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonImageSectionLeft),IntToStr(132),nil);
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonImageSectionTop),IntToStr(73),nil);
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(128,96,96),6)),nil);

         UpdateSettings(-Ord(stGraphicsMenuFocusedEnabledButtonFontColor),RGB_BGR(Misc_.IntToHex(RGB(255,255,255),6)),nil);
         UpdateSettings(-Ord(stGraphicsMenuFocusedEnabledButtonTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(128,96,96),6)),nil);

         UpdateSettings(-Ord(stGraphicsMenuPopupMenuColorsButton),RGB_BGR(Misc_.IntToHex(RGB(255,128,0),6)),nil);

         UpdateSettings(-Ord(stGraphicsStatusImageSectionLeft),IntToStr(132),nil);
         UpdateSettings(-Ord(stGraphicsStatusImageSectionTop),IntToStr(73),nil);
         UpdateSettings(-Ord(stGraphicsStatusTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(128,96,96),6)),nil);
         UpdateSettings(-Ord(stGraphicsStatusSubTextsFontColor),RGB_BGR(Misc_.IntToHex(DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_ORANGE,6)),nil);
         end;
       ctRed : begin
         SetColorTheme( ctBlue );
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonImageSectionLeft),IntToStr(132),nil);
         UpdateSettings(-Ord(stGraphicsMenuEnabledButtonTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(128,96,96),6)),nil);

         UpdateSettings(-Ord(stGraphicsMenuFocusedEnabledButtonFontColor),RGB_BGR(Misc_.IntToHex(RGB(192,0,0),6)),nil);

         UpdateSettings(-Ord(stGraphicsMenuPopupMenuColorsButton),RGB_BGR(Misc_.IntToHex(RGB(192,0,0),6)),nil);

         UpdateSettings(-Ord(stGraphicsStatusImageSectionLeft),IntToStr(132),nil);
         UpdateSettings(-Ord(stGraphicsStatusTextShadowColor),RGB_BGR(Misc_.IntToHex(RGB(128,96,96),6)),nil);
         UpdateSettings(-Ord(stGraphicsStatusSubTextsFontColor),RGB_BGR(Misc_.IntToHex(DEFAULT_STATUS_PANEL_SUBTEXT_FONT_COLOR_RED,6)),nil);
         end;
     end;
end;

procedure TOptionsForm.MenuItemBuiltinButtonsClick(Sender: TObject);
var CloseAction:TCloseAction;
begin //
  ModalResult:=mrNone;
  if CloseEditors(False) and
     (TreeView1.Items.Count=Succ(Ord(High(TSettings)))) then with TreeView1 do begin // sanity check, just to be sure that all items have been created
     Update;

     if      Sender=MenuItemBuiltinBlueButtons   then SetColorTheme( ctBlue   )
     else if Sender=MenuItemBuiltinGreenButtons  then SetColorTheme( ctGreen  )
     else if Sender=MenuItemBuiltinOrangeButtons then SetColorTheme( ctOrange )
     else if Sender=MenuItemBuiltinRedButtons    then SetColorTheme( ctRed    );

     Modified:=True; ActiveControl:=OKBtn;
     ModalResult:=mrOk;
     if   Screen.ActiveForm=Self then Close
     else FormClose(Self,CloseAction);
     ModalResult:=mrOk;
     end;
end;

function  TOptionsForm.IsAFileNameSetting(TreeNode__:TTreeNode):Boolean;
begin
  Result:=(TreeNode__<>nil) and StrEqual(TreeNode__.Text,FileNameText);
end;

function  TOptionsForm.IsAPictureSetting(Index__:Integer; var PictureType__:TPictureType):Boolean;
var i,j:Integer; p:TPictureType;
begin // caution: hard-wired to work with the current items in the tree-structure
  Result:= (Index__>=Ord(stGraphicsBoardFigures)) and
           (Index__<=Ord(stGraphicsBoardFiguresFloorVisible));
  if Result then begin
     i:=High(i); // picture types aren't sequentially ordered, hence, look for the closest one
     for p:=Low(p) to ptBoxOnGoalAnimation do begin
         j:=Index__-PictureSettingsOffset[p];
         if (j>=0) and (j<i) then begin
            i:=j; PictureType__:=p;
            end;
         end;
     end;
end;

function  TOptionsForm.CommitOrDropChanges:Boolean;
begin
  Result:=CloseEditors(False) and
          OpenForm.CloseText(tTextFile) and
          OpenForm.CloseText(tSkinScript);
  if Result then
     if Modified then
        case Msg(SettingsChangedText,
                 Caption,
                 MB_ICONQUESTION+MB_YESNOCANCEL) of
          IDYES   : begin SaveData(True);
                          LoadData; // 'LoadData' is necessary because 'SaveData' may change a few things
                    end;
          IDNO    : LoadData;
          IDCANCEL: Result:=False;
        end; // case
end;

procedure TOptionsForm.Panel1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  ShowHint(Sender);
end;

procedure TOptionsForm.MenuItemCopyToClipboardClick(Sender: TObject);
var Text,TempFileName:String; FileTime:TFileTime;
begin
  TempFileName:=MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,WORK_FILE_NAME_EXT));
  if SaveToFile(TempFileName,False) then
     try     if   LoadTextFromFile(Text,FileTime,TempFileName) then
                  try    ClipBoard.AsText:=Text;
                         StatusBar1.Panels[1].Text:=SettingsCopiedToClipboardText;
                  except on E:Exception do Error(E.Message,Caption);
                  end
             else Error(TEXT_TASK_FAILED,Caption);
     finally SysUtils.DeleteFile(TempFileName);
     end;
end;

procedure TOptionsForm.MenuItemPasteFromClipboardFullClick(Sender: TObject);
var s,TempFileName:String; F:TextFile;
begin
  if   Clipboard.HasFormat(CF_TEXT) then
       try    s:=ClipBoard.AsText;
              TempFileName:=MainForm.ApplicationDataPath+ExtractFileName(ChangeFileExt(Application.ExeName,WORK_FILE_NAME_EXT));
              AssignFile(F,TempFileName);
              Rewrite(F);
              try     Write(F,s);
              finally CloseFile(F);
                      try     LoadFromFile(TempFileName,Sender<>MenuItemPasteFromClipboardSupplementary);
                      finally SysUtils.DeleteFile(TempFileName);
                      end;
              end;
       except on E:Exception do Error(E.Message,Caption);
       end
  else begin MenuItemPasteFromClipboardFull.Enabled:=Clipboard.HasFormat(CF_TEXT);
             MenuItemPasteFromClipboardSupplementary.Enabled:=MenuItemPasteFromClipboardFull.Enabled;
             Msg(TEXT_CLIPBOARD_NO_TEXT,Caption,MB_OK+MB_ICONINFORMATION);
       end;
end;

procedure TOptionsForm.MenuItemEditClick(Sender: TObject);
begin
  ShowStatus; {update 'paste from clipboard' enabled/disabled}
end;

function  TOptionsForm.IsOpenFormAvailable:Boolean;
begin
  Result:=not (Assigned(OpenForm) and OpenForm.Visible);
  if not Result then Msg(OpenFormNotAvailableText,Caption,MB_OK+MB_ICONINFORMATION);
end;

procedure TOptionsForm.MenuItemDefaultSkinWallTypeClick(Sender: TObject);
begin
  if Sender<>MenuItemDefaultSkinBrickWalls then begin
     if HasDefaultSkinWall or HasDefaultSkinBrickWall then begin
        SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBoardFiguresWall)]);
        SetDefaultSkinMenuItemCheckMark(Sender);
        if   MenuItemDefaultSkinSeamlessWalls.Checked then
             fSettingsString[stGraphicsBoardFiguresWallType]:=WallTypeText[wtSeamlessWallWithCap]
        else fSettingsString[stGraphicsBoardFiguresWallType]:=WallTypeText[wtTiledWall];
        if   MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked then begin
             SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBackground)]);
             SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBoardBackground)]);
             MenuItemBuiltinButtonsClick(MenuItemBuiltinBlueButtons);
             end
        else MenuItemBuiltinButtonsClick(nil);
        end
     else SetDefaultSkinMenuItemCheckMark(Sender);
     end
  else
     if HasDefaultSkinWall or HasDefaultSkinBrickWall then begin
        SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBoardFiguresWall)]);
        SetDefaultSkinMenuItemCheckMark(Sender);
        FSettingsString[stGraphicsBoardFiguresWallImageSectionLeft ]:=IntToStr(DEFAULT_SKIN_BRICK_WALL_LEFT);
        FSettingsString[stGraphicsBoardFiguresWallImageSectionTop  ]:=IntToStr(DEFAULT_SKIN_BRICK_WALL_TOP );
        FSettingsString[stGraphicsBoardFiguresWallImageAntiAliasing]:=AntiAliasingText[aaFilter];
        if   MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked then begin
             SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBackground)]);
             SetToDefaultValue(TreeView1.Items[Ord(stGraphicsBoardBackground)]);             
             MenuItemBuiltinButtonsClick(MenuItemBuiltinBlueButtons);
             end
        else MenuItemBuiltinButtonsClick(nil);
        end
     else SetDefaultSkinMenuItemCheckMark(Sender);
end;

function TOptionsForm.HasDefaultSkinWall:Boolean;
var s:TSettings;
begin
  Result:=True;
  for s:=stGraphicsBoardFiguresWallImageFileName to stGraphicsBoardFiguresWallImageSectionHeight do
      if SettingsEditor[s]<>seNone then
         Result:=Result and StrEqual(SettingsString[s],DefaultSettingsString[s]);
end;

function TOptionsForm.HasDefaultSkinBrickWall:Boolean;
begin
  Result:=StrEqual(SettingsString[stGraphicsBoardFiguresWallImageFileName],DefaultSettingsString[stGraphicsBoardFiguresWallImageFileName])
          and
          StrEqual(SettingsString[stGraphicsBoardFiguresWallImageSectionLeft  ],IntToStr(DEFAULT_SKIN_BRICK_WALL_LEFT))
          and
          StrEqual(SettingsString[stGraphicsBoardFiguresWallImageSectionTop   ],IntToStr(DEFAULT_SKIN_BRICK_WALL_TOP))
          and
          StrEqual(SettingsString[stGraphicsBoardFiguresWallImageSectionWidth ],DefaultSettingsString[stGraphicsBoardFiguresWallImageSectionWidth ])
          and
          StrEqual(SettingsString[stGraphicsBoardFiguresWallImageSectionHeight],DefaultSettingsString[stGraphicsBoardFiguresWallImageSectionHeight])
          ;
end;

procedure TOptionsForm.SetDefaultSkinMenuItemCheckMark(Sender: TObject);
begin
  MenuItemDefaultSkinSeamlessWalls.Checked:=Sender=MenuItemDefaultSkinSeamlessWalls;
  MenuItemDefaultSkinTiledWalls   .Checked:=Sender=MenuItemDefaultSkinTiledWalls;
  MenuItemDefaultSkinBrickWalls   .Checked:=Sender=MenuItemDefaultSkinBrickWalls;
end;

procedure TOptionsForm.MenuItemDefaultSkinDefaultBackgroundAndButtonsClick(
  Sender: TObject);
begin
  with MenuItemDefaultSkinDefaultBackgroundAndButtons do Checked:=not Checked;
  ShowDefaultSkinSubMenu;
end;

function  TOptionsForm.ShowDefaultSkinSubMenu:Boolean;
var Char1,Char2:Char;
begin
  Result:=StrHasAcceleratorChar(MenuItemSkins      .Caption,True,Char1) and
          StrHasAcceleratorChar(MenuItemDefaultSkin.Caption,True,Char2);
  if Result then begin
     keybd_event( VK_MENU,    MapVirtualKey( VK_MENU,    0), 0, 0 );
     keybd_event( Ord(Char1), MapVirtualKey( Ord(Char1), 0), 0, 0 );
     keybd_event( Ord(Char2), MapVirtualKey( Ord(Char2), 0), 0, 0 );
     end;
end;

function TOptionsForm.LoadSettingsFromIniFile(const IniFile: TIniFile): Boolean;
var i:Integer;
begin // LoadSettingsFromIniFile;
  MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked:=IniFile.ReadBool(OPTIONS_FORM_INIFILE_SECTION,'DefaultSkinDefaultBackgroundAndButtons',MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked);
  i:=IniFile.ReadInteger(OPTIONS_FORM_INIFILE_SECTION,'SkinExportFormatType',Ord(SkinExportFormatType));
  if (i>=Ord(Low(TSkinExportFormatType))) and (i<=Ord(High(TSkinExportFormatType))) then
     SkinExportFormatType:=TSkinExportFormatType(i);
  MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified :=IniFile.ReadString (OPTIONS_FORM_INIFILE_SECTION,'MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified',MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified);
  Result :=LoadFontFromIniFile(IniFile,OPTIONS_FORM_INIFILE_SECTION,'Window',Self.Font);
  OnFontChange;
end;

function TOptionsForm.SaveSettingsToIniFile(const IniFile: TIniFile): Boolean;
begin // SaveSettingsToIniFile;
  try    IniFile.WriteBool(OPTIONS_FORM_INIFILE_SECTION,'DefaultSkinDefaultBackgroundAndButtons',MenuItemDefaultSkinDefaultBackgroundAndButtons.Checked);
         IniFile.WriteInteger(OPTIONS_FORM_INIFILE_SECTION,'SkinExportFormatType',Ord(SkinExportFormatType));
         IniFile.WriteString (OPTIONS_FORM_INIFILE_SECTION,'MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified',MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified);
         Result:=SaveFontToIniFile(IniFile,OPTIONS_FORM_INIFILE_SECTION,'Window',Self.Font);
  except on E:Exception do Result:=Error(E.Message,Application.Title);
  end;
end;

function  TOptionsForm.MakeSkin(var {o:} SkinBitMap__ : TBitMap; var {o:} ColCount__, RowCount__ : Integer ): Boolean;
const
  SOKOBAN_YASC_DEFAULT_SKIN_SETTINGS =
    'Wall type=3x2';
  SOKOBAN_YASC_SKIN_SETTINGS_DIRECTIONAL_PLAYER =
    'Directional player';
  SKIN_SETTINGS_BACKGROUND_COLOR_RGB_COMPONENT = 96; // darker than the color "clGray", which is RGB(127,127,127)
var
  W, H, Col, MaxTextLength : Integer;
  Direction : TDirection;
  Quadrant : TQuadrantType;
  p : TPictureType;
  oAntialiazing:TAntialiasing;
  oCursor : TCursor;
  Text, ErrorText, SkinSettingsText : String;

  function  MakeGrayObjectBackground( Object__, Background__ : TPictureType ) : Boolean;
  var R : TRect;
      B :TBitMap;
  begin
    with MainForm.GamePictures do begin
      Result := Assigned( Pictures[ Object__ ].BitMap ) and
                Assigned( Pictures[ Background__ ].BitMap ) and
                ( Pictures[ Object__ ].BitMap.Width = Pictures[ Background__ ].BitMap.Width ) and
                ( Pictures[ Object__ ].BitMap.Height = Pictures[ Background__ ].BitMap.Height ) and
                BitMapResize( Pictures[ Object__ ].MaskBitMap,Pictures[ Object__ ].BitMap.Width,Pictures[ Object__ ].BitMap.Height ) and
                BitMapCreate( B, Pictures[ Object__ ].BitMap.Width, Pictures[ Object__ ].BitMap.Height );
      if Result then with Pictures[ Object__ ] do
         try  R := Rect( 0, 0, BitMap.Width, BitMap.Height );
              B.Canvas.CopyRect( R, Pictures[ Object__ ].BitMap.Canvas, R ); // copy bitmap to "B"
              //BitMapMaskBackgroundForAllPixels( B, MaskBitMap, Pictures[ Background__ ].BitMap, MaskBitMapPct, False );
              BitMapMaskBackgroundForAllPixels_HueAndSaturationOnly( B, MaskBitMap, Pictures[ Background__ ].BitMap, MaskBitMapPct, False ); // create mask and zero out masked pixels in the "B" bitmap itself
              BitMapGrayImage( B );
              BitMap.Canvas.CopyMode := cmSrcAnd; // and-ind mask
              BitMap.Canvas.CopyRect( R, Pictures[ Object__ ].MaskBitMap.Canvas, R );
              BitMap.Canvas.CopyMode := cmSrcPaint; // or-ing object, i.e., the gray box
              BitMap.Canvas.CopyRect( R, B.Canvas, R );
              EdgeSmoothing( BitMap, BitMap, MaskBitMap, R, True, True ); // try to make the foreground/background edge more smooth
         finally B.Free;
         end;
      end;
  end;

  function  MakeSkinSettingsText( var Text__ : String ) : Boolean;
  type TNodeFlag = ( nfAlwaysExport, nfOnlyExportIfDefault, nfPictureStart, nfVisited );
       TNodeFlags = set of TNodeFlag;
  var  p : TPictureType;
       Setting : TSettings;
       //oCursor:TCursor;
       NodeFlags:array[TSettings] of TNodeFlags;
       Node,Next:TTreeNode;
       SL:TStringList;

    procedure AddSectionHeader(Node__:TTreeNode);
    begin
      if (Node__<>nil) and (not ( nfVisited in NodeFlags[TSettings(Node__.AbsoluteIndex)])) then
         with Node__ do begin
           //AddSectionHeader(Parent);
           SL.Add('');
           SL.Add(LEFT_BRACKET+FullItemName(Node__)+RIGHT_BRACKET);
           Include( NodeFlags[TSettings(Node__.AbsoluteIndex)], nfVisited );
           end;
    end;

  begin // MakeSkinSettingsText: creates a text with all non-default settings values for the skin
    Result := True;
    Text__:='';
    //oCursor:=Screen.Cursor;
    try    SL:=TStringList.Create;
           try     //Screen.Cursor:=crHourGlass;
                   if (TreeView1.Items.Count =Succ( Ord( High( Setting ) ) - Ord( Low( Setting ) ) ) ) and // sanity check
                      (stGraphicsBoardGridColor2 < High( Setting ) ) // sanity check, guarding against range error
                      then begin
                      FillChar( NodeFlags, SizeOf( NodeFlags ), 0 );
                      // cooporate with the "common skins format" import script
                      // in order to minimize the number of required explicit
                      // settings
                      for p := ptPlayer to ptBoxOnGoalMoveAnimation do begin
                          Setting := TSettings( PictureSettingsOffset[ p ] );
                          Include( NodeFlags[ Setting ], nfPictureStart );
                          Include( NodeFlags[ TSettings( Ord( Setting ) + Ord( stGraphicsBoardFiguresPlayerImageMasked ) - Ord( stGraphicsBoardFiguresPlayer ) ) ], nfOnlyExportIfDefault );
                          if ( p = ptGoal ) or ( p = ptWall ) or ( p = ptFloor ) then
                             Include( NodeFlags[ TSettings( Ord( Setting ) + Ord( stGraphicsBoardFiguresPlayerImageAntiAliasing ) - Ord( stGraphicsBoardFiguresPlayer ) ) ], nfOnlyExportIfDefault );
                          end;
                      Include( NodeFlags[ stGraphicsBoardFiguresFloorVisible ], nfOnlyExportIfDefault );

                      if StrEqual( SettingsString[ stSmoothMoveAnimationEnabled ], BooleanText[ False ] ) then
                         with TreeView1.Items[ Ord( stSmoothMoveAnimationEnabled ) ] do begin
                         AddSectionHeader( Parent );
                         SL.Add( Text + EQUAL + BooleanText[ False ] );
                         end;
                      // for simplicity, always export the skin without animations.
                      // if this is changed, then the code for building the exported
                      // skin bitmap must be updated accordingly
                      for Setting := stPathFindingShowLegalMovesAnimationPlayerAnimationEnabled to stPathFindingShowLegalMovesAnimationBoxAnimationEnabled do
                          with TreeView1.Items[ Ord( Setting ) ] do begin
                            AddSectionHeader( Parent );
                            SL.Add( Text + EQUAL + BooleanText[ False ] );
                            end;
                      for Setting := stSolutionsAnimationPlayerAnimationEnabled to stSolutionsAnimationBoxAnimationEnabled do
                          with TreeView1.Items[ Ord( Setting ) ] do begin
                            AddSectionHeader( Parent );
                            SL.Add( Text + EQUAL + BooleanText[ False ] );
                            end;

                      Setting := stMoveAnimationForkliftDriving;
                      while Setting <= stGraphicsBoardGridColor2 do begin
                        if nfPictureStart in NodeFlags[ Setting ] then // 'True': skip file name and image section
                           Setting := TSettings( Ord( Setting ) + Succ( Ord( stGraphicsBoardFiguresPlayerImageSectionHeight ) - Ord( stGraphicsBoardFiguresPlayer ) ) );
                        Node:=TreeView1.Items[Ord(Setting)];
                        with Node do begin
                          if (not HasChildren)
                             and
                             ( ( ( SettingsString[ Setting ]   <> DefaultSettingsString[ Setting ] )
                                 and
                                 ( not ( nfOnlyExportIfDefault in NodeFlags[ Setting ] ) )
                               )
                               or
                               ( ( nfOnlyExportIfDefault       in NodeFlags[ Setting ] )
                                 and
                                 ( SettingsString[ Setting ]   =  DefaultSettingsString[ Setting ] )
                               )
                               or
                               ( nfAlwaysExport in NodeFlags[ Setting ] )
                             )
                             and
                             (SettingsEditor[Setting]<>seNone)
                             then begin
                             AddSectionHeader(Parent);
                             SL.Add(Text+EQUAL+SettingsString[Setting]);
                             end;
                          if Setting<High(Setting) then begin
                             if   Setting =  stMoveAnimationForkliftDrivingDriveInReverseSquares then
                                  Setting := stGraphicsBoardFigures
                             else Inc( Setting );
                             Next:=TreeView1.Items[Ord(Setting)];
                             if (Next.Level<Node.Level) and (Next.Parent<>nil) then
                                // stepping one level back;
                                // clear 'Visited' for the parent of the next node
                                // in order to trigger a section header for the next node
                                Exclude(NodeFlags[TSettings(Next.Parent.AbsoluteIndex)], nfVisited );
                             end;
                          end;
                        end;
                      end
                   else raise Exception.Create(TEXT_TASK_FAILED); // shouldn't happen

                   Text__ := SL.Text; // return the text
           finally //Screen.Cursor:=oCursor;
                   SL.Free;
           end;
    except on E:Exception do Result:=Error(E.Message, '' );
    end;
  end;

begin // MakeSkin: exports the currently loaded skin as a simple 4 columns x 8
      // rows skin in "Common Skins Format". Directional player imagery, if any,
      // is included, but player animations and box animations are ignored.
      // postcondition: the caller is responsible for destroying the created
      // skin bitmap.
  SkinBitMap__ := nil; ColCount__ := 0; RowCount__ := 0;
  Result := (not IsBusy) and CloseEditors(False) and MenuItemExportSkin.Enabled  and CommitOrDropChanges and MainForm.GamePictures.Initialized;
  if Result then with MainForm.GamePictures do begin
     MenuItemExportSkin.Tag := 1; // 'Tag <> 0' : 'Export skin...' has invalidated the loaded game pictures and the game-viewers
     MainForm.GameViewer.Clear;                 // kludge: all game-viewers must be cleared before calling 'GamePictures.LoadPictures'
     OpenForm.GameViewer.Clear;
     OpenForm.GameViewer.Canvas:=nil;  // forces the game-viewer to rebuild background images the next time the window is opened
     Result := LoadPictures; // ensure that the skin has been loaded with the current settings in the 'Settings' window
     oCursor := Screen.Cursor;
     if Result then
        try
          try
            Screen.Cursor := crHourglass;
            W := 1; H := 1; ErrorText := '';

            ColCount__ := 4; RowCount__ := 4; // base format, columns x rows: 4 x 4
            if MainForm.GamePictures.FrameCount[ ptPlayer ] = DIRECTION_COUNT then
               Inc( RowCount__, 2 ); // format, columns x rows: 4 x 6
            if SkinExportformatType = seft4x8 then // 'True': fill up to the common skins default format, 4 x 8 columns and rows, even though the exported skin doesn't include imagery for animations
               RowCount__ := 8;

            for p := ptPlayer to ptFloor do
                if Assigned( Pictures[ p ] ) and Assigned( Pictures[ p ].OrgBitMap ) then with Pictures[ p ] do begin // find the largest tile size. all tiles will be normalized to this size.
                   if Pictures[ p ].Visible then begin
                      W := Max( W, OrgBitMap.Width  div Max( 1, MainForm.GamePictures.FrameCount[ p ] ) );
                      H := Max( H, OrgBitMap.Height );
                      end;
                   end
                else Result := False;

            if Result then
               for p := ptPlayer to ptFloor do with Pictures[ p ] do begin // make all tiles the same size
                   if   ( W = OrgBitMap.Width  div Max( 1, MainForm.GamePictures.FrameCount[ p ] ) ) and
                        ( H = OrgBitMap.Height div Max( 1, MainForm.GamePictures.FrameCount[ p ] ) ) then begin
                        Result := Result and BitMapResize( BitMap, OrgBitMap.Width, OrgBitMap.Height );
                        if Result then with BitMap do with Canvas do begin
                           CopyMode := cmSrcCopy;
                           CopyRect( Rect( 0, 0, Width, Height ), OrgBitMap.Canvas, Rect( 0, 0, Width, Height ) );
                           end;
                        end
                   else Result := Result and ResizeFrames( W, H, MainForm.GamePictures.FrameCount[ p ], nil );
                   end;

            if Result and
               BitMapCreate( SkinBitMap__, W * ColCount__, H * RowCount__ ) then with SkinBitMap__ do with Canvas do begin
               Brush.Style := bsSolid;
               Brush.Color := clGray;
               FillRect( Rect( 0 , 0, Width, Height ) );
               CopyMode := cmSrcCopy;

               CopyRect( CellToRect( 0, 0, W, H ), Pictures[ ptFloor ].BitMap.Canvas, Rect( 0, 0, W, H ) );
               CopyRect( CellToRect( 0, 1, W, H ), Pictures[ ptGoal  ].BitMap.Canvas, Rect( 0, 0, W, H ) );
               if   MainForm.GamePictures.FrameCount[ ptPlayer ] <> DIRECTION_COUNT then
                    CopyRect( CellToRect( 1, 0, W, H ), Pictures[ ptPlayer ].BitMap.Canvas, Rect( 0, 0, W, H ) )
               else CopyRect( CellToRect( 1, 0, W, H ), Pictures[ ptPlayer ].BitMap.Canvas, CellToRect( Ord( DEFAULT_PLAYER_DIRECTION ), 0, W, H ) );
               if   MainForm.GamePictures.FrameCount[ ptPlayerOnGoal ] <> DIRECTION_COUNT then
                    CopyRect( CellToRect( 1, 1, W, H ), Pictures[ ptPlayerOnGoal ].BitMap.Canvas, Rect( 0, 0, W, H ) )
               else CopyRect( CellToRect( 1, 1, W, H ), Pictures[ ptPlayerOnGoal ].BitMap.Canvas, CellToRect( Ord( DEFAULT_PLAYER_DIRECTION ), 0, W, H ) );
               CopyRect( CellToRect( 2, 0, W, H ), Pictures[ ptBox       ].BitMap.Canvas, Rect( 0, 0, W, H ) );
               CopyRect( CellToRect( 2, 1, W, H ), Pictures[ ptBoxOnGoal ].BitMap.Canvas, Rect( 0, 0, W, H ) );

               if FrameCount[ ptWall ] = WALL_TILE_COUNT then begin
                  CopyRect( CellToRect( 0, 2, W, H ), Pictures[ ptWall ].BitMap.Canvas, CellToRect( 15, 0, W, H ) ); // walls left, above, right, below
                  CopyRect( CellToRect( 1, 2, W, H ), Pictures[ ptWall ].BitMap.Canvas, CellToRect( 10, 0, W, H ) ); // walls left,        right
                  CopyRect( CellToRect( 0, 3, W, H ), Pictures[ ptWall ].BitMap.Canvas, CellToRect( 05, 0, W, H ) ); // walls       above,        below
                  CopyRect( CellToRect( 1, 3, W, H ), Pictures[ ptWall ].BitMap.Canvas, CellToRect( 00, 0, W, H ) ); // no wall neighbors
                  CopyRect( CellToRect( 2, 2, W, H ), Pictures[ ptWall ].BitMap.Canvas, CellToRect( 16, 0, W, H ) ); // wall cap
                  for Quadrant := Low( Quadrant ) to High( Quadrant ) do // wall cap
                      CopyRect( RectQuadrant( CellToRect( 2, 2, W, H ), Quadrant ),
                                Pictures[ ptWall ].BitMap.Canvas,
                                RectQuadrant( CellToRect( 16, 0, W, H ), TQuadrantType( Ord( High( Quadrant ) ) - Ord( Quadrant ) ) ) ); // the wall bitmap stores the wall cap quadrants in reversed order, thereby making the wall cap ready for display
                  end
               else begin
                  CopyRect( CellToRect( 0, 2, W, H ), Pictures[ ptWall ].BitMap.Canvas, Rect( 0, 0, W, H ) ); // walls left, above, right, below
                  CopyRect( CellToRect( 1, 2, W, H ), Pictures[ ptWall ].BitMap.Canvas, Rect( 0, 0, W, H ) ); // walls left,        right
                  CopyRect( CellToRect( 0, 3, W, H ), Pictures[ ptWall ].BitMap.Canvas, Rect( 0, 0, W, H ) ); // walls       above,        below
                  CopyRect( CellToRect( 1, 3, W, H ), Pictures[ ptWall ].BitMap.Canvas, Rect( 0, 0, W, H ) ); // no wall neighbors
                  CopyRect( CellToRect( 2, 2, W, H ), Pictures[ ptWall ].BitMap.Canvas, Rect( 0, 0, W, H ) ); // wall cap
                  end;

               if MainForm.GamePictures.FrameCount[ ptPlayer ] = DIRECTION_COUNT then begin // 'True': make the skin with directional player imagery
                  for Direction := Low( Direction ) to High( Direction ) do begin
                      CopyRect( CellToRect( Ord( Direction ), 4, W, H ), Pictures[ ptPlayer       ].BitMap.Canvas, CellToRect( Ord( Direction ), 0, W, H ) );
                      CopyRect( CellToRect( Ord( Direction ), 5, W, H ), Pictures[ ptPlayerOnGoal ].BitMap.Canvas, CellToRect( Ord( Direction ), 0, W, H ) );
                      end;
                  end
               else // no directional player
                  if RowCount__ >= 6 then
                     for Col := 0 to Pred( ColCount__ ) do begin // make placeholders for a player animation
                         CopyRect( CellToRect( Col, 4, W, H ), Pictures[ ptPlayer       ].BitMap.Canvas, Rect( 0, 0, W, H ) );
                         CopyRect( CellToRect( Col, 5, W, H ), Pictures[ ptPlayerOnGoal ].BitMap.Canvas, Rect( 0, 0, W, H ) );
                         end;

               if RowCount__ > 6 then
                  for Col := 0 to Pred( ColCount__ ) do begin // make placeholders for a box animation
                      CopyRect( CellToRect( Col, RowCount__ - 2, W, H ), Pictures[ ptBox       ].BitMap.Canvas, Rect( 0, 0, W, H ) );
                      CopyRect( CellToRect( Col, RowCount__ - 1, W, H ), Pictures[ ptBoxOnGoal ].BitMap.Canvas, Rect( 0, 0, W, H ) );
                      end;

               //BitMapGrayImage( Pictures[ ptBox       ].BitMap );
               //BitMapGrayImage( Pictures[ ptBoxOnGoal ].BitMap );
               //CopyRect( CellToRect( 3, 0, W, H ), Pictures[ ptBox       ].BitMap.Canvas, Rect( 0, 0, W, H ) );
               //CopyRect( CellToRect( 3, 1, W, H ), Pictures[ ptBoxOnGoal ].BitMap.Canvas, Rect( 0, 0, W, H ) );

               if MakeGrayObjectBackground( ptBox, ptFloor ) then
                  CopyRect( CellToRect( 3, 0, W, H ), Pictures[ ptBox       ].BitMap.Canvas, Rect( 0, 0, W, H ) );
               if MakeGrayObjectBackground( ptBoxOnGoal, ptGoal ) then
                  CopyRect( CellToRect( 3, 1, W, H ), Pictures[ ptBoxOnGoal ].BitMap.Canvas, Rect( 0, 0, W, H ) );


               with Pictures[ ptScreenBackground ] do
                 if Visible and
                    ( View = ivTile ) and
                    Assigned( OrgBitMap ) and
                    ( OrgBitMap.Width  <= 3 * W ) and // "3 *": a heuristic: if the background image isn't much larger than the tile size, then include the background image in the exported skin
                    ( OrgBitMap.Height <= 3 * H ) then begin
                    oAntialiazing:=Antialiasing;
                    AntiAliasing:=aaFilter; // best scaling quality
                    if Resize( W, H ) then
                       CopyRect( CellToRect( 3, 2, W, H ), BitMap.Canvas, Rect( 0, 0, W, H ) );
                    Antialiasing:=oAntialiazing;
                    end;

               Text := SOKOBAN_YASC_DEFAULT_SKIN_SETTINGS;
               if MainForm.GamePictures.FrameCount[ ptPlayer ] = DIRECTION_COUNT then
                  Text := SOKOBAN_YASC_SKIN_SETTINGS_DIRECTIONAL_PLAYER + EQUAL+ AnsiLowerCase( TEXT_YES ) + NL + Text;
               if MakeSkinSettingsText( SkinSettingsText ) and ( Length( SkinSettingsText ) > 0 ) then
                  Text := Text + NL + SkinSettingsText;
               MaxTextLength := MaxBitMapTextLength( SkinBitMap__, CellToRect( 3, 3, W, H ) );
               if Length( Text ) > MaxTextLength then // clip the text to the length which can be stored in the bitmap. saving at least some of settings is better than nothing
                  Text := Copy( Text, 1, MaxTextLength );

               Brush.Color := RGBToColor( __RGB( SKIN_SETTINGS_BACKGROUND_COLOR_RGB_COMPONENT, SKIN_SETTINGS_BACKGROUND_COLOR_RGB_COMPONENT, SKIN_SETTINGS_BACKGROUND_COLOR_RGB_COMPONENT ) );
               FillRect( CellToRect( 3, 3, W, H ) );
               Result := Result and SetBitMapText(SkinBitMap__, CellToRect( 3, 3, W, H ), Text, ErrorText );
               end
            else Result := False;

            if not Result then begin
               if ErrorText = '' then
                  ErrorText := TEXT_TASK_FAILED;
               Error(ErrorText,'');
               end;
          except on E : Exception do Result := Error(E.Message,'');
          end;
        finally Screen.Cursor := oCursor;
                try     Result := LoadPictures and Result; // load the current skin again
                finally if not Result then begin
                           SkinBitMap__.Free; SkinBitMap__ := nil;
                           end;
                end;
        end
     else Error( TEXT_TASK_FAILED, '' );
     end;
end;

procedure TOptionsForm.MenuItemExportSkinClick(Sender: TObject);
var
  ColCount, RowCount : Integer;
  s, s1 : String;
  SkinBitMap : TBitMap;
begin
  SkinBitMap := nil;
  try     if Assigned( CaptureForm ) and
             MakeSkin( SkinBitMap, ColCount, RowCount ) then with CaptureForm do with SaveDialog do begin
             //BitMapDump( SkinBitMap );
             //BitMapColorChange( SkinBitMap, 120, 0, 0 );
             //BitMapDump( SkinBitMap );
             //BitMapColorChange( SkinBitMap, 120, 0, 0 );
             //BitMapDump( SkinBitMap );
             Filter := BitmapImagesText + '  (' + BITMAP_FILES_FILTER + RIGHT_PAREN + BAR + BITMAP_FILES_FILTER;
             FilterIndex := 1;
             Title := Self.Caption + SUB_TITLE_SEPARATOR + TEXT_SAVE_SKIN_CAPTION;

             if SkinInitialDirectory = '' then
                SkinInitialDirectory := StrWithoutTrailingPathDelimiter( MainForm.Skins.DefaultSkinPath );
             InitialDir := SkinInitialDirectory;

             if ( InitialDir = '' ) or ( not DirectoryExists( InitialDir ) ) then
                InitialDir := StrWithoutTrailingPathDelimiter( MainForm.ApplicationDataPath );
             if ( InitialDir = '' ) or ( not DirectoryExists( InitialDir ) ) then
                InitialDir := StrWithoutTrailingPathDelimiter(GetFolderPath(CSIDL_PERSONAL));

             if Trim( DefaultSkinFileNameText ) = '' then
                DefaultSkinFileNameText := SkinText;
             s := MostRecentlyLoadedSkinTitleIfTheSettingsHaveNotBeenModified;
             if s =  '' then
                s := DefaultSkinFileNameText;
             s1 := BMP_FILE_EXT;
             s := MakeNewFileName( StrWithTrailingPathDelimiter( InitialDir ) + s, s1, True );
             if   s <> '' then begin
                  FileName:=StrToFileName(ExtractFileName(s)); // excluding path
                  if Execute then begin
                     FileName := ChangeFileExt( FileName, BMP_FILE_EXT );
                     if True or // 'True': allow the file name proper to be empty, e.g., "..\skins\.bmp"
                        ( ExtractFileNameWithoutExt( FileName ) <> '' ) then begin
                        if DirectoryExists(FileName) then begin
                           Msg(Format(DirectoryExistsText__,[ FileName]),Title,MB_OK+MB_ICONINFORMATION);
                           end
                        else
                           if FileExists( FileName ) and
                              ( Msg( Format( FileExistsText__, [ AbbreviatedFilePath( FileName, MainForm.MyDocumentsFolder ) ] ) + NL + NL + OverwriteItText,
                                     Title,
                                     MB_YESNO + MB_ICONQUESTION + MB_DEFBUTTON2 )
                                <> IDYES ) then begin
                              // cancel save operation
                              end
                           else begin
                              InitialDir := StrWithoutTrailingPathDelimiter( ExtractFilePath( FileName ) );
                              SkinInitialDirectory := InitialDir;
                              try    DrawSkinTitle( SkinBitMap, ColCount, RowCount, ExtractFileNameWithoutPathAndExtension( FileName ) );
                                     SkinBitmap.SaveToFile( FileName );
                              except on E:Exception do Error( E.Message, '' );
                              end;
                              end;
                        end
                     else Msg(Format(EmptyFileNameText__+NL+NL+TEXT_TASK_FAILED,[ AbbreviatedFilePath( FileName, MainForm.MyDocumentsFolder ) ]),Title,MB_OK+MB_ICONINFORMATION);
                     end;
                  end
             else Error(DiskFullText, '');
             end;
  finally SkinBitMap.Free;
  end;
end;

procedure TOptionsForm.TreeViewWndProc(var Msg: TMessage);
begin // hides the horizontal scrollbar. it itn't useful because the right side of it is hidden under the string-grid with the data.
  ShowScrollBar(TreeView1.Handle, SB_HORZ, False);
//ShowScrollBar(TreeView1.Handle, SB_VERT, True );
  FTreeViewWndProc(Msg);
end;

procedure TOptionsForm.OnFontChange;
begin
  StatusBar1.Font.Assign(Self.Font);
  TopPanel.Font.Name:=Self.Font.Name;
  TopPanel.Font.Style:=Self.Font.Style;
  TopPanel.Font.Size:=TOP_PANEL_FONT_SIZE;
end;

end.




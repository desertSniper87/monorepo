package com.newsblur.activity;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.database.Cursor;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.app.LoaderManager;
import android.support.v4.content.Loader;
import android.support.v4.view.ViewPager;
import android.support.v4.view.ViewPager.OnPageChangeListener;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.ProgressBar;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView;
import android.widget.Toast;

import butterknife.ButterKnife;
import butterknife.Bind;

import com.newsblur.R;
import com.newsblur.database.ReadingAdapter;
import com.newsblur.domain.Story;
import com.newsblur.fragment.ReadingItemFragment;
import com.newsblur.fragment.ReadingPagerFragment;
import com.newsblur.service.NBSyncService;
import com.newsblur.util.AppConstants;
import com.newsblur.util.DefaultFeedView;
import com.newsblur.util.FeedSet;
import com.newsblur.util.FeedUtils;
import com.newsblur.util.PrefConstants.ThemeValue;
import com.newsblur.util.PrefsUtils;
import com.newsblur.util.ReadingFontChangedListener;
import com.newsblur.util.StateFilter;
import com.newsblur.util.UIUtils;
import com.newsblur.util.ViewUtils;
import com.newsblur.util.VolumeKeyNavigation;
import com.newsblur.view.ReadingScrollView.ScrollChangeListener;

public abstract class Reading extends NbActivity implements OnPageChangeListener, OnSeekBarChangeListener, ScrollChangeListener, LoaderManager.LoaderCallbacks<Cursor>, ReadingFontChangedListener {

    public static final String EXTRA_FEEDSET = "feed_set";
	public static final String EXTRA_POSITION = "feed_position";
    public static final String EXTRA_STORY_HASH = "story_hash";
    private static final String BUNDLE_POSITION = "position";
    private static final String BUNDLE_STARTING_UNREAD = "starting_unread";
    private static final String BUNDLE_SELECTED_FEED_VIEW = "selectedFeedView";
    private static final String BUNDLE_IS_FULLSCREEN = "is_fullscreen";

    /** special value for starting story hash that jumps to the first unread. */
    public static final String FIND_FIRST_UNREAD = "FIND_FIRST_UNREAD";

    private static final float OVERLAY_ELEVATION_DP = 1.5f;
    private static final int OVERLAY_RANGE_TOP_DP = 40;
    private static final int OVERLAY_RANGE_BOT_DP = 60;

    /** The minimum screen width (in DP) needed to show all the overlay controls. */
    private static final int OVERLAY_MIN_WIDTH_DP = 355;

	protected StateFilter intelState;

    // Activities navigate to a particular story by hash.
    // We can find it once we have the cursor.
    private String storyHash;

    protected final Object STORIES_MUTEX = new Object();
	protected Cursor stories;

    @Bind(android.R.id.content) View contentView; // we use this a ton, so cache it
    @Bind(R.id.reading_overlay_left) Button overlayLeft;
    @Bind(R.id.reading_overlay_right) Button overlayRight;
    @Bind(R.id.reading_overlay_progress) ProgressBar overlayProgress;
    @Bind(R.id.reading_overlay_progress_right) ProgressBar overlayProgressRight;
    @Bind(R.id.reading_overlay_progress_left) ProgressBar overlayProgressLeft;
    @Bind(R.id.reading_overlay_text) Button overlayText;
    @Bind(R.id.reading_overlay_send) Button overlaySend;
    @Bind(R.id.reading_empty_view_text) View emptyViewText;
    @Bind(R.id.reading_sync_status) TextView overlayStatusText;
    
    ViewPager pager;
    ReadingPagerFragment readingFragment;

	protected ReadingAdapter readingAdapter;
    private boolean stopLoading;
    protected FeedSet fs;

    // unread count for the circular progress overlay. set to nonzero to activate the progress indicator overlay
    protected int startingUnreadCount = 0;

    private float overlayRangeTopPx;
    private float overlayRangeBotPx;

    private int lastVScrollPos = 0;

    private boolean unreadSearchActive = false;

    private List<Story> pageHistory;

    private VolumeKeyNavigation volumeKeyNavigation;

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        setIntent(intent);
    }

    @Override
	protected void onCreate(Bundle savedInstanceBundle) {
        super.onCreate(savedInstanceBundle);

		setContentView(R.layout.activity_reading);
        ButterKnife.bind(this);

        try {
            fs = (FeedSet)getIntent().getSerializableExtra(EXTRA_FEEDSET);
        } catch (RuntimeException re) {
            // in the wild, the notification system likes to pass us an Intent that has missing or very stale
            // Serializable extras.
            com.newsblur.util.Log.e(this, "failed to unfreeze required extras", re);
            finish();
            return;
        }

        if (fs == null) {
            com.newsblur.util.Log.w(this.getClass().getName(), "reading view had no FeedSet");
            finish();
            return;
        }

        if ((savedInstanceBundle != null) && savedInstanceBundle.containsKey(BUNDLE_STARTING_UNREAD)) {
            startingUnreadCount = savedInstanceBundle.getInt(BUNDLE_STARTING_UNREAD);
        }

        // Only use the storyHash the first time the activity is loaded. Ignore when
        // recreated due to rotation etc.
        if (savedInstanceBundle == null) {
            storyHash = getIntent().getStringExtra(EXTRA_STORY_HASH);
        } else {
            storyHash = savedInstanceBundle.getString(EXTRA_STORY_HASH);
        }

		intelState = PrefsUtils.getStateFilter(this);
        volumeKeyNavigation = PrefsUtils.getVolumeKeyNavigation(this);

        // were we fullscreen before rotation?
        if ((savedInstanceBundle != null) && savedInstanceBundle.containsKey(BUNDLE_IS_FULLSCREEN)) {
            boolean isFullscreen = savedInstanceBundle.getBoolean(BUNDLE_IS_FULLSCREEN, false);
            if (isFullscreen) {
                ViewUtils.hideSystemUI(getWindow().getDecorView());
            }
        }

        // this value is expensive to compute but doesn't change during a single runtime
        this.overlayRangeTopPx = (float) UIUtils.dp2px(this, OVERLAY_RANGE_TOP_DP);
        this.overlayRangeBotPx = (float) UIUtils.dp2px(this, OVERLAY_RANGE_BOT_DP);

        this.pageHistory = new ArrayList<Story>();

        ViewUtils.setViewElevation(overlayLeft, OVERLAY_ELEVATION_DP);
        ViewUtils.setViewElevation(overlayRight, OVERLAY_ELEVATION_DP);
        ViewUtils.setViewElevation(overlayText, OVERLAY_ELEVATION_DP);
        ViewUtils.setViewElevation(overlaySend, OVERLAY_ELEVATION_DP);
        ViewUtils.setViewElevation(overlayProgress, OVERLAY_ELEVATION_DP);
        ViewUtils.setViewElevation(overlayProgressLeft, OVERLAY_ELEVATION_DP);
        ViewUtils.setViewElevation(overlayProgressRight, OVERLAY_ELEVATION_DP);

        // this likes to default to 'on' for some platforms
        enableProgressCircle(overlayProgressLeft, false);
        enableProgressCircle(overlayProgressRight, false);

		FragmentManager fragmentManager = getSupportFragmentManager();
		ReadingPagerFragment fragment = (ReadingPagerFragment) fragmentManager.findFragmentByTag(ReadingPagerFragment.class.getName());
		if (fragment == null) {
            fragment = ReadingPagerFragment.newInstance();
			FragmentTransaction transaction = fragmentManager.beginTransaction();
			transaction.add(R.id.activity_reading_container, fragment, ReadingPagerFragment.class.getName());
			transaction.commit();
		}

        getSupportLoaderManager().initLoader(0, null, this);
	}

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        if (storyHash != null) {
            outState.putString(EXTRA_STORY_HASH, storyHash);
        } else {
            if (pager != null) {
                int currentItem = pager.getCurrentItem();
                Story story = readingAdapter.getStory(currentItem);
                if (story != null ) {
                    outState.putString(EXTRA_STORY_HASH, story.storyHash);
                }
            }
        }

        if (startingUnreadCount != 0) {
            outState.putInt(BUNDLE_STARTING_UNREAD, startingUnreadCount);
        }

        if (ViewUtils.isSystemUIHidden(getWindow().getDecorView())) {
            outState.putBoolean(BUNDLE_IS_FULLSCREEN, true);
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (NBSyncService.isHousekeepingRunning()) finish();
        // this view shows stories, it is not safe to perform cleanup
        this.stopLoading = false;
        // this is not strictly necessary, since our first refresh with the fs will swap in
        // the correct session, but that can be delayed by sync backup, so we try here to
        // reduce UI lag, or in case somehow we got redisplayed in a zero-story state
        FeedUtils.prepareReadingSession(fs, false);
    }

    @Override
    protected void onPause() {
        this.stopLoading = true;
        super.onPause();
    }

	@Override
	public Loader<Cursor> onCreateLoader(int loaderId, Bundle bundle) {
        if (fs == null) {
            Log.e(this.getClass().getName(), "can't create activity, no feedset ready");
            // this is probably happening in a finalisation cycle or during a crash, pop the activity stack
            finish();
            return null;
        }
        return FeedUtils.dbHelper.getActiveStoriesLoader(fs);
    }

	@Override
	public void onLoaderReset(Loader<Cursor> loader) {
	}

	@Override
	public void onLoadFinished(Loader<Cursor> loader, Cursor cursor) {
        synchronized (STORIES_MUTEX) {
            if (cursor == null) return;
            if (stopLoading) return;

            if (! FeedUtils.dbHelper.isFeedSetReady(fs)) {
                com.newsblur.util.Log.i(this.getClass().getName(), "stale load");
                // the system can and will re-use activities, so during the initial mismatch of
                // data, don't show the old stories
                pager.setVisibility(View.INVISIBLE);
                stories = null;
                triggerRefresh(AppConstants.READING_STORY_PRELOAD);
                return;
            }

            if (readingAdapter != null) {
                // swapCursor() will asynch process the new cursor and fully update the pager,
                // update child fragments, and then call pagerUpdated()
                readingAdapter.swapCursor(cursor, pager);
            }

            stories = cursor;
            
            com.newsblur.util.Log.d(this.getClass().getName(), "loaded cursor with count: " + cursor.getCount());
            if (cursor.getCount() < 1) {
                triggerRefresh(AppConstants.READING_STORY_PRELOAD);
            }
        }
	}

    /**
     * notify the activity that the dataset for the pager has fully been updated
     */
    public void pagerUpdated() {
        // see if we are just starting and need to jump to a target story
        skipPagerToStoryHash();

        if (unreadSearchActive) {
            // if we left this flag high, we were looking for an unread, but didn't find one;
            // now that we have more stories, look again.
            nextUnread();
        }
        updateOverlayNav();
        updateOverlayText();
    }

    private void skipPagerToStoryHash() {
        // if we already started and found our target story, this will be unset
        if (storyHash == null) return;
        int position = -1;
        if (storyHash.equals(FIND_FIRST_UNREAD)) {
            position = readingAdapter.findFirstUnread();
        } else {
            position = readingAdapter.findHash(storyHash);
        }

        if (stopLoading) return;

        if (position >= 0 ) {
            pager.setCurrentItem(position, false);
            this.onPageSelected(position);
            // now that the pager is getting the right story, make it visible
            pager.setVisibility(View.VISIBLE);
            emptyViewText.setVisibility(View.INVISIBLE);
            storyHash = null;
            return;
        }

        // if the story wasn't found, try to get more stories into the cursor
        this.checkStoryCount(readingAdapter.getCount()+1);
    }

    /*
     * The key component of this activity is the pager, which in order to correctly use
     * child fragments for stories, needs to be within an enclosing fragment.  Because
     * the view heirarchy of that fragment will have a different lifecycle than the
     * activity, we need a way to get access to the pager when it is created and only
     * then can we set it up.
     */
    public void offerPager(ViewPager pager, FragmentManager childFragmentManager) {
        this.pager = pager;

        // since it might start on the wrong story, create the pager as invisible
        pager.setVisibility(View.INVISIBLE);
		pager.setPageMargin(UIUtils.dp2px(getApplicationContext(), 1));

        ThemeValue themeValue = PrefsUtils.getSelectedTheme(this);
        if (themeValue == ThemeValue.LIGHT) {
            pager.setPageMarginDrawable(R.drawable.divider_light);
        } else if (themeValue == ThemeValue.DARK) {
            pager.setPageMarginDrawable(R.drawable.divider_dark);
        }

        boolean showFeedMetadata = true;
        if (fs.isSingleNormal()) showFeedMetadata = false;
        String sourceUserId = null;
        if (fs.getSingleSocialFeed() != null) sourceUserId = fs.getSingleSocialFeed().getKey();
        readingAdapter = new ReadingAdapter(childFragmentManager, sourceUserId, showFeedMetadata, this);
        
		pager.setAdapter(readingAdapter);

        // if the first story in the list was "viewed" before the page change listener was set,
        // the calback was probably missed
        if (storyHash == null) {
            this.onPageSelected(pager.getCurrentItem());
        }

        updateOverlayNav();
        enableOverlays();
	}

    /**
     * Query the DB for the current unreadcount for this view.
     */
    private int getUnreadCount() {
        // saved stories and global shared stories don't have unreads
        if (fs.isAllSaved() || fs.isGlobalShared()) return 0;
        int result = FeedUtils.dbHelper.getUnreadCount(fs, intelState);
        if (result < 0) return 0;
        return result;
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.reading, menu);
		return true;
	}

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        super.onPrepareOptionsMenu(menu);
        return true;
    }

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		if (item.getItemId() == android.R.id.home) {
			finish();
			return true;
		} else if (item.getItemId() == R.id.menu_reading_fullscreen) {
            ViewUtils.hideSystemUI(getWindow().getDecorView());
            return true;
        } else {
			return super.onOptionsItemSelected(item);
		}
	}

    @Override
	protected void handleUpdate(int updateType) {
        if ((updateType & UPDATE_REBUILD) != 0) {
            finish();
        }
        if ((updateType & UPDATE_STATUS) != 0) {
            enableMainProgress(NBSyncService.isFeedSetSyncing(this.fs, this));
            if (overlayStatusText != null) {
                String syncStatus = NBSyncService.getSyncStatusMessage(this, true);
                if (syncStatus != null)  {
                    if (AppConstants.VERBOSE_LOG) {
                        syncStatus = syncStatus + UIUtils.getMemoryUsageDebug(this);
                    }
                    overlayStatusText.setText(syncStatus);
                    overlayStatusText.setVisibility(View.VISIBLE);
                } else {
                    overlayStatusText.setVisibility(View.GONE);
                }
            }
        }
        if ((updateType & UPDATE_STORY) != 0) {    
            updateCursor();
            updateOverlayNav();
        }
        
        ReadingItemFragment item = getReadingFragment();
        if (item != null) {
            item.handleUpdate(updateType);
        }
    }

    private void updateCursor() {
        synchronized (STORIES_MUTEX) {
            try {
                getSupportLoaderManager().restartLoader(0, null, this);
            } catch (IllegalStateException ise) {
                ; // our heavy use of async can race loader calls, which it will gripe about, but this
                 //  is only a refresh call, so dropping a refresh during creation is perfectly fine.
            }
        }
    }

    // interface OnPageChangeListener

	@Override
	public void onPageScrollStateChanged(int arg0) {
	}

	@Override
	public void onPageScrolled(int arg0, float arg1, int arg2) {
	}

	@Override
	public void onPageSelected(final int position) {
        new AsyncTask<Void, Void, Void>() {
            @Override
            protected Void doInBackground(Void... params) {
                if (readingAdapter == null) return null;
                Story story = readingAdapter.getStory(position);
                if (story != null) {
                    FeedUtils.markStoryAsRead(story, Reading.this);
                    synchronized (pageHistory) {
                        // if the history is just starting out or the last entry in it isn't this page, add this page
                        if ((pageHistory.size() < 1) || (!story.equals(pageHistory.get(pageHistory.size()-1)))) {
                            pageHistory.add(story);
                        }
                    }
                }
                checkStoryCount(position);
                updateOverlayText();
                enableOverlays();
                return null;
            }
        }.execute();
	}

    // interface ScrollChangeListener

    @Override
    public void scrollChanged(int hPos, int vPos, int currentWidth, int currentHeight) {
        // only update overlay alpha every few pixels. modern screens are so dense that it
        // is way overkill to do it on every pixel
        if (Math.abs(lastVScrollPos-vPos) < 2) return;
        lastVScrollPos = vPos;

        int scrollMax = currentHeight - contentView.getMeasuredHeight();
        int posFromBot = (scrollMax - vPos);

        float newAlpha = 0.0f;
        if ((vPos < this.overlayRangeTopPx) && (posFromBot < this.overlayRangeBotPx)) {
            // if we have a super-tiny scroll window such that we never leave either top or bottom,
            // just leave us at full alpha.
            newAlpha = 1.0f;
        } else if (vPos < this.overlayRangeTopPx) {
            float delta = this.overlayRangeTopPx - ((float) vPos);
            newAlpha = delta / this.overlayRangeTopPx;
        } else if (posFromBot < this.overlayRangeBotPx) {
            float delta = this.overlayRangeBotPx - ((float) posFromBot);
            newAlpha = delta / this.overlayRangeBotPx;
        }
        
        this.setOverlayAlpha(newAlpha);
    }

    private void setOverlayAlpha(final float a) {
        // check to see if the device even has room for all the overlays, moving some to overflow if not
        int widthPX = contentView.getMeasuredWidth();
        boolean overflowExtras = false;
        if (widthPX != 0) {
            float widthDP = UIUtils.px2dp(this, widthPX);
            if ( widthDP < OVERLAY_MIN_WIDTH_DP ){
                overflowExtras = true;
            } 
        }

        final boolean _overflowExtras = overflowExtras;
        runOnUiThread(new Runnable() {
            public void run() {
                UIUtils.setViewAlpha(overlayLeft, a, true);
                UIUtils.setViewAlpha(overlayRight, a, true);
                UIUtils.setViewAlpha(overlayProgress, a, true);
                UIUtils.setViewAlpha(overlayText, a, true);
                UIUtils.setViewAlpha(overlaySend, a, !_overflowExtras);
            }
        });
    }

    /**
     * Make visible and update the overlay UI.
     */
    public void enableOverlays() {
        this.setOverlayAlpha(1.0f);
    }

    public void disableOverlays() {
        this.setOverlayAlpha(0.0f);
    }

    /**
     * Update the next/back overlay UI after the read-state of a story changes or we navigate in any way.
     */
    private void updateOverlayNav() {
        int currentUnreadCount = getUnreadCount();
        if (currentUnreadCount > this.startingUnreadCount ) {
            this.startingUnreadCount = currentUnreadCount;
        }
        this.overlayLeft.setEnabled(this.getLastReadPosition(false) != -1);
        this.overlayRight.setText((currentUnreadCount > 0) ? R.string.overlay_next : R.string.overlay_done);
        if (currentUnreadCount > 0) {
            this.overlayRight.setBackgroundResource(UIUtils.getThemedResource(this, R.attr.selectorOverlayBackgroundRight, android.R.attr.background));
        } else {
            this.overlayRight.setBackgroundResource(UIUtils.getThemedResource(this, R.attr.selectorOverlayBackgroundRightDone, android.R.attr.background));
        }

        if (this.startingUnreadCount == 0 ) {
            // sessions with no unreads just show a full progress bar
            this.overlayProgress.setMax(1);
            this.overlayProgress.setProgress(1);
        } else {
            int unreadProgress = this.startingUnreadCount - currentUnreadCount;
            this.overlayProgress.setMax(this.startingUnreadCount);
            this.overlayProgress.setProgress(unreadProgress);
        }
        this.overlayProgress.invalidate();

        invalidateOptionsMenu();
    }

     private void updateOverlayText() {
        if (overlayText == null) return;
        runOnUiThread(new Runnable() {
            public void run() {
                ReadingItemFragment item = getReadingFragment();
                if (item == null) return;
                if (item.getSelectedViewMode() == DefaultFeedView.STORY) {
                    overlayText.setBackgroundResource(UIUtils.getThemedResource(Reading.this, R.attr.selectorOverlayBackgroundText, android.R.attr.background));
                    overlayText.setText(R.string.overlay_text);
                } else {
                    overlayText.setBackgroundResource(UIUtils.getThemedResource(Reading.this, R.attr.selectorOverlayBackgroundStory, android.R.attr.background));
                    overlayText.setText(R.string.overlay_story);
                }
            }
        });
    }

    public void onWindowFocusChanged(boolean hasFocus) {
        // this callback is a good API-level-independent way to tell when the root view size/layout changes
        super.onWindowFocusChanged(hasFocus);
        this.contentView = findViewById(android.R.id.content);
        // Ensure that we come out of immersive view if the activity no longer has focus
        if (!hasFocus) {
            ViewUtils.showSystemUI(getWindow().getDecorView());
        }
    }

	/**
     * While navigating the story list and at the specified position, see if it is possible
     * and desirable to start loading more stories in the background.  Note that if a load
     * is triggered, this method will be called again by the callback to ensure another
     * load is not needed and all latches are tripped.
     */
    private void checkStoryCount(int position) {
        if (stories == null ) {
			triggerRefresh(position + AppConstants.READING_STORY_PRELOAD);
        } else {
            if (AppConstants.VERBOSE_LOG) {
                Log.d(this.getClass().getName(), String.format("story %d of %d selected, stopLoad: %b", position, stories.getCount(), stopLoading));
            }
            // if the pager is at or near the number of stories loaded, check for more unless we know we are at the end of the list
            if ((position + AppConstants.READING_STORY_PRELOAD) >= stories.getCount()) {
                triggerRefresh(position + AppConstants.READING_STORY_PRELOAD);
            }
        }
	}

	protected void enableMainProgress(boolean enabled) {
        enableProgressCircle(overlayProgressRight, enabled);
	}

    public void enableLeftProgressCircle(boolean enabled) {
        enableProgressCircle(overlayProgressLeft, enabled);
    }

    private void enableProgressCircle(final ProgressBar view, final boolean enabled) {
        runOnUiThread(new Runnable() {
            public void run() {
                if (enabled) {
                    view.setProgress(0);
                    view.setVisibility(View.VISIBLE);
                } else {
                    view.setProgress(100);
                    view.setVisibility(View.GONE);
                }
            }
        });
	}
        
	private void triggerRefresh(int desiredStoryCount) {
		if (!stopLoading) {
            Integer currentCount = null;
            if (stories != null) currentCount = stories.getCount();
            boolean gotSome = NBSyncService.requestMoreForFeed(fs, desiredStoryCount, currentCount);
            if (gotSome) triggerSync();
		}
    }

    // NB: this callback is for the text size slider
	@Override
	public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        float size = AppConstants.READING_FONT_SIZE[progress];
	    PrefsUtils.setTextSize(this, size);
		Intent data = new Intent(ReadingItemFragment.TEXT_SIZE_CHANGED);
		data.putExtra(ReadingItemFragment.TEXT_SIZE_VALUE, size); 
		sendBroadcast(data);
	}

	@Override
	public void onStartTrackingTouch(SeekBar seekBar) {
	}

	@Override
	public void onStopTrackingTouch(SeekBar seekBar) {
	}

	@Override
    public void readingFontChanged(String newValue) {
        PrefsUtils.setFontString(this, newValue);
        sendBroadcast(new Intent(ReadingItemFragment.READING_FONT_CHANGED));
    }

    /**
     * Click handler for the righthand overlay nav button.
     */
    public void overlayRight(View v) {
        if (getUnreadCount() <= 0) {
            // if there are no unread stories, go back to the feed list
            Intent i = new Intent(this, Main.class);
            i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(i);
            finish();
        } else {
            // if there are unreads, go to the next one
            new AsyncTask<Void, Void, Void>() {
                @Override
                protected Void doInBackground(Void... params) {
                    nextUnread();
                    return null;
                }
            }.execute();
        }
    }

    /**
     * Search our set of stories for the next unread one. 
     */
    private void nextUnread() {
        unreadSearchActive = true;

        // if we somehow got tapped before construction or are running during destruction, stop and
        // let either finish. search will happen when the cursor is pushed.
        if ((pager == null) || (readingAdapter == null)) return;

        boolean unreadFound = false;
        // start searching just after the current story
        int currentIndex = pager.getCurrentItem();
        int candidate = currentIndex + 1;
        unreadSearch:while (!unreadFound) {
            // if we've reached the end of the list, start searching backward from the current story
            if (candidate >= readingAdapter.getCount()) {
                candidate = currentIndex - 1;
            }
            // if we have looked all the way back to the first story, there aren't any left
            if (candidate < 0) {
                break unreadSearch;
            }
            Story story = readingAdapter.getStory(candidate);
            if (this.stopLoading) {
                // this activity was ended before we finished. just stop.
                unreadSearchActive = false;
                return;
            } 
            // iterate through the stories in our cursor until we find an unread one
            if (story != null) {
                if (story.read) {
                    if (candidate > currentIndex ) {
                        // if we are still searching past the current story, search forward
                        candidate++;
                    } else {
                        // if we hit the end and re-started before the current story, search backward
                        candidate--;
                    }
                    continue unreadSearch;
                } else {
                    unreadFound = true;
                }
            }
            // if we didn't continue or break, the cursor probably changed out from under us, so stop.
            break unreadSearch;
        }

        if (unreadFound) {
            // jump to the story we found
            final int page = candidate;
            runOnUiThread(new Runnable() {
                public void run() {
                    pager.setCurrentItem(page, true);
                }
            });
            // disable the search flag, as we are done
            unreadSearchActive = false;
        } else {
            // We didn't find a story, so we should trigger a check to see if the API can load any more.
            // First, though, double check that there are even any left, as there may have been a delay
            // between marking an earlier one and double-checking counts.
            if (getUnreadCount() <= 0) {
                unreadSearchActive = false;
            } else {
                // trigger a check to see if there are any more to search before proceeding. By leaving the
                // unreadSearchActive flag high, this method will be called again when a new cursor is loaded
                this.checkStoryCount(readingAdapter.getCount()+1);
            }
        }
    }

    /**
     * Click handler for the lefthand overlay nav button.
     */
    public void overlayLeft(View v) {
        int targetPosition = this.getLastReadPosition(true);
        if (targetPosition != -1) {
            pager.setCurrentItem(targetPosition, true);
        } else {
            Log.e(this.getClass().getName(), "reading history contained item not found in cursor.");
        }
    }

    /**
     * Get the pager position of the last story read during this activity or -1 if there is nothing
     * in the history.
     *
     * @param trimHistory optionally trim the history of the currently displayed page iff the
     *        back button has been pressed.
     */
    private int getLastReadPosition(boolean trimHistory) {
        synchronized (this.pageHistory) {
            // the last item is always the currently shown page, do not count it
            if (this.pageHistory.size() < 2) {
                return -1;
            }
            Story targetStory = this.pageHistory.get(this.pageHistory.size()-2);
            int targetPosition = this.readingAdapter.getPosition(targetStory);
            if (trimHistory && (targetPosition != -1)) {
                this.pageHistory.remove(this.pageHistory.size()-1);
            }
            return targetPosition;
        }
    }

    /**
     * Click handler for the progress indicator on the righthand overlay nav button.
     */
    public void overlayCount(View v) {
        String unreadText = getString((getUnreadCount() == 1) ? R.string.overlay_count_toast_1 : R.string.overlay_count_toast_N);
        Toast.makeText(this, String.format(unreadText, getUnreadCount()), Toast.LENGTH_SHORT).show();
    }

    public void overlaySend(View v) {
        if ((readingAdapter == null) || (pager == null)) return;
		Story story = readingAdapter.getStory(pager.getCurrentItem());
        FeedUtils.sendStoryBrief(story, this);
    }

    public void overlayText(View v) {
        final ReadingItemFragment item = getReadingFragment();
        if (item == null) return;
        new AsyncTask<Void, Void, Void>() {
            @Override
            protected Void doInBackground(Void... params) {
                item.switchSelectedViewMode();
                return null;
            }
        }.execute();
    }

    private ReadingItemFragment getReadingFragment() {
        if (readingAdapter == null || pager == null) { return null; }
        return readingAdapter.getExistingItem(pager.getCurrentItem());
    }

    public FeedSet getFeedSet() {
        return this.fs;
    }

    public void viewModeChanged() {
        ReadingItemFragment frag = readingAdapter.getExistingItem(pager.getCurrentItem());
        frag.viewModeChanged();
        // fragments to the left or the right may have already preloaded content and need to also switch
        frag = readingAdapter.getExistingItem(pager.getCurrentItem()-1);
        if (frag != null) frag.viewModeChanged();
        frag = readingAdapter.getExistingItem(pager.getCurrentItem()+1);
        if (frag != null) frag.viewModeChanged();
        updateOverlayText();
    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {
        if (isVolumeKeyNavigationEvent(keyCode)) {
            processVolumeKeyNavigationEvent(keyCode);
            return true;
        } else {
            return super.onKeyDown(keyCode, event);
        }
    }

    private boolean isVolumeKeyNavigationEvent(int keyCode) {
        return volumeKeyNavigation != VolumeKeyNavigation.OFF &&
               (keyCode == KeyEvent.KEYCODE_VOLUME_DOWN || keyCode == KeyEvent.KEYCODE_VOLUME_UP);
    }

    private void processVolumeKeyNavigationEvent(int keyCode) {
        if ((keyCode == KeyEvent.KEYCODE_VOLUME_DOWN && volumeKeyNavigation == VolumeKeyNavigation.DOWN_NEXT) ||
            (keyCode == KeyEvent.KEYCODE_VOLUME_UP && volumeKeyNavigation == VolumeKeyNavigation.UP_NEXT)) {
            if (pager == null) return;
            int nextPosition = pager.getCurrentItem() + 1;
            if (nextPosition < readingAdapter.getCount()) {
                try {
                    pager.setCurrentItem(nextPosition);
                } catch (Exception e) {
                    // Just in case cursor changes.
                }
            }
        } else {
            if (pager == null) return;
            int nextPosition = pager.getCurrentItem() - 1;
            if (nextPosition >= 0) {
                try {
                    pager.setCurrentItem(nextPosition);
                } catch (Exception e) {
                    // Just in case cursor changes.
                }
            }
        }
    }

    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        // Required to prevent the default sound playing when the volume key is pressed
        if (isVolumeKeyNavigationEvent(keyCode)) {
            return true;
        } else {
            return super.onKeyUp(keyCode, event);
        }
    }
}

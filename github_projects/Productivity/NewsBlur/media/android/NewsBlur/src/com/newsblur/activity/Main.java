package com.newsblur.activity;

import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.widget.SwipeRefreshLayout;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnKeyListener;
import android.widget.AbsListView;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.PopupMenu;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView;

import butterknife.ButterKnife;
import butterknife.Bind;
import butterknife.OnClick;

import com.newsblur.R;
import com.newsblur.fragment.FeedIntelligenceSelectorFragment;
import com.newsblur.fragment.FolderListFragment;
import com.newsblur.fragment.LoginAsDialogFragment;
import com.newsblur.fragment.LogoutDialogFragment;
import com.newsblur.fragment.TextSizeDialogFragment;
import com.newsblur.service.BootReceiver;
import com.newsblur.service.NBSyncService;
import com.newsblur.util.AppConstants;
import com.newsblur.util.FeedUtils;
import com.newsblur.util.PrefConstants.ThemeValue;
import com.newsblur.util.PrefsUtils;
import com.newsblur.util.StateFilter;
import com.newsblur.util.UIUtils;
import com.newsblur.view.StateToggleButton.StateChangedListener;

public class Main extends NbActivity implements StateChangedListener, SwipeRefreshLayout.OnRefreshListener, AbsListView.OnScrollListener, PopupMenu.OnMenuItemClickListener, OnSeekBarChangeListener {

    public static final String EXTRA_FORCE_SHOW_FEED_ID = "force_show_feed_id";

	private FolderListFragment folderFeedList;
	private FragmentManager fragmentManager;
    private SwipeRefreshLayout swipeLayout;
    private boolean wasSwipeEnabled = false;
    @Bind(R.id.main_sync_status) TextView overlayStatusText;
    @Bind(R.id.empty_view_image) ImageView emptyViewImage;
    @Bind(R.id.empty_view_text) TextView emptyViewText;
    @Bind(R.id.main_menu_button) Button menuButton;
    @Bind(R.id.main_user_image) ImageView userImage;
    @Bind(R.id.main_user_name) TextView userName;
    @Bind(R.id.main_unread_count_neut_text) TextView unreadCountNeutText;
    @Bind(R.id.main_unread_count_posi_text) TextView unreadCountPosiText;
    @Bind(R.id.feedlist_search_query) EditText searchQueryInput;

    @Override
	public void onCreate(Bundle savedInstanceState) {
        PreferenceManager.setDefaultValues(this, R.xml.activity_settings, false);

		super.onCreate(savedInstanceState);
        getWindow().setBackgroundDrawableResource(android.R.color.transparent);

		setContentView(R.layout.activity_main);
        ButterKnife.bind(this);

		getActionBar().hide();

        // set the status bar to an generic loading message when the activity is first created so
        // that something is displayed while the service warms up
        overlayStatusText.setText(R.string.loading);
        overlayStatusText.setVisibility(View.VISIBLE);

        swipeLayout = (SwipeRefreshLayout)findViewById(R.id.swipe_container);
        swipeLayout.setColorSchemeResources(R.color.refresh_1, R.color.refresh_2, R.color.refresh_3, R.color.refresh_4);
        swipeLayout.setProgressBackgroundColorSchemeResource(UIUtils.getThemedResource(this, R.attr.actionbarBackground, android.R.attr.background));
        swipeLayout.setOnRefreshListener(this);

		fragmentManager = getSupportFragmentManager();
		folderFeedList = (FolderListFragment) fragmentManager.findFragmentByTag("folderFeedListFragment");
		folderFeedList.setRetainInstance(true);
        ((FeedIntelligenceSelectorFragment) fragmentManager.findFragmentByTag("feedIntelligenceSelector")).setState(folderFeedList.currentState);

        // make sure the interval sync is scheduled, since we are the root Activity
        BootReceiver.scheduleSyncService(this);

        Bitmap userPicture = PrefsUtils.getUserImage(this);
        if (userPicture != null) {
            userPicture = UIUtils.clipAndRound(userPicture, 5, false);
            userImage.setImageBitmap(userPicture);
        }
        userName.setText(PrefsUtils.getUserDetails(this).username);
        searchQueryInput.setOnKeyListener(new OnKeyListener() {
            public boolean onKey(View v, int keyCode, KeyEvent event) {
                if ((keyCode == KeyEvent.KEYCODE_BACK) && (event.getAction() == KeyEvent.ACTION_DOWN)) {
                    searchQueryInput.setVisibility(View.GONE);
                    searchQueryInput.setText("");
                    checkSearchQuery();
                    return true;
                }
                if ((keyCode == KeyEvent.KEYCODE_ENTER) && (event.getAction() == KeyEvent.ACTION_DOWN)) {
                    checkSearchQuery();
                    return true;
                }   
                return false;
            }
        });
        searchQueryInput.addTextChangedListener(new TextWatcher() {
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                checkSearchQuery();
            }
            public void afterTextChanged(Editable s) {}
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
        });
	}

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        setIntent(intent);
    }

    @Override
    protected void onResume() {
        try {
            // due to weird backstack operations coming from notified reading activities,
            // sometimes we fail to reload. do everything in our power to log
            super.onResume();
        } catch (Exception e) {
            com.newsblur.util.Log.e(getClass().getName(), "error resuming Main", e);
            finish();
        }

        String forceShowFeedId = getIntent().getStringExtra(EXTRA_FORCE_SHOW_FEED_ID);
        if (forceShowFeedId != null) {
            folderFeedList.forceShowFeed(forceShowFeedId);
        }

        if (folderFeedList.getSearchQuery() != null) {
            searchQueryInput.setText(folderFeedList.getSearchQuery());
            searchQueryInput.setVisibility(View.VISIBLE);
        }

        // triggerSync() might not actually do enough to push a UI update if background sync has been
        // behaving itself. because the system will re-use the activity, at least one update on resume
        // will be required, however inefficient
        folderFeedList.hasUpdated();

        NBSyncService.resetReadingSession(FeedUtils.dbHelper);
        NBSyncService.flushRecounts();

        updateStatusIndicators();
        folderFeedList.pushUnreadCounts();
        folderFeedList.checkOpenFolderPreferences();
        triggerSync();
    }

	@Override
	public void changedState(StateFilter state) {
        if ( !( (state == StateFilter.ALL) ||
                (state == StateFilter.SOME) ||
                (state == StateFilter.BEST) ) ) {
            searchQueryInput.setText("");
            searchQueryInput.setVisibility(View.GONE);
            checkSearchQuery();
        }

		folderFeedList.changeState(state);
	}
	
    @Override
	public void handleUpdate(int updateType) {
        if ((updateType & UPDATE_REBUILD) != 0) {
            folderFeedList.reset();
        }
        if ((updateType & UPDATE_DB_READY) != 0) {
            try {
                folderFeedList.startLoaders();
            } catch (IllegalStateException ex) {
                ; // this might be called multiple times, and startLoaders is *not* idempotent
            }
        }
        if ((updateType & UPDATE_STATUS) != 0) {
            updateStatusIndicators();
        }
		if ((updateType & UPDATE_METADATA) != 0) {
            folderFeedList.hasUpdated();
        }
	}

    public void updateUnreadCounts(int neutCount, int posiCount) {
        unreadCountNeutText.setText(Integer.toString(neutCount));
        unreadCountPosiText.setText(Integer.toString(posiCount));
    }

    /**
     * A callback for the feed list fragment so it can tell us how many feeds (not folders)
     * are being displayed based on mode, etc.  This lets us adjust our wrapper UI without
     * having to expensively recalculate those totals from the DB.
     */
    public void updateFeedCount(int feedCount) {
        if (feedCount < 1 ) {
            if (NBSyncService.isFeedCountSyncRunning() || (!folderFeedList.firstCursorSeenYet)) {
                emptyViewImage.setVisibility(View.INVISIBLE);
                emptyViewText.setVisibility(View.INVISIBLE);
            } else {
                emptyViewImage.setVisibility(View.VISIBLE);
                if (folderFeedList.currentState == StateFilter.BEST) {
                    emptyViewText.setText(R.string.empty_list_view_no_focus_stories);
                } else if (folderFeedList.currentState == StateFilter.SAVED) {
                    emptyViewText.setText(R.string.empty_list_view_no_saved_stories);
                } else {
                    emptyViewText.setText(R.string.empty_list_view_no_unread_stories);
                }
                emptyViewText.setVisibility(View.VISIBLE);
            }
        } else {
            emptyViewImage.setVisibility(View.INVISIBLE);
            emptyViewText.setVisibility(View.INVISIBLE);
        }
    }

    private void updateStatusIndicators() {
        if (NBSyncService.isFeedFolderSyncRunning()) {
            swipeLayout.setRefreshing(true);
        } else {
            swipeLayout.setRefreshing(false);
        }

        if (overlayStatusText != null) {
            String syncStatus = NBSyncService.getSyncStatusMessage(this, false);
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

    @Override
    public void onRefresh() {
        NBSyncService.forceFeedsFolders();
        triggerSync();
        folderFeedList.clearRecents();
    }

    @OnClick(R.id.main_menu_button) void onClickMenuButton() {
        PopupMenu pm = new PopupMenu(this, menuButton);
        Menu menu = pm.getMenu();
        pm.getMenuInflater().inflate(R.menu.main, menu);

        MenuItem loginAsItem = menu.findItem(R.id.menu_loginas);
        if (NBSyncService.isStaff == Boolean.TRUE) {
            loginAsItem.setVisible(true);
        } else {
            loginAsItem.setVisible(false);
        }

        MenuItem feedbackItem = menu.findItem(R.id.menu_feedback);
        if (AppConstants.ENABLE_FEEDBACK) {
            feedbackItem.setTitle(feedbackItem.getTitle() + " (v" + PrefsUtils.getVersion(this) + ")");
        } else {
            feedbackItem.setVisible(false);
        }

        if ( (folderFeedList.currentState == StateFilter.ALL) ||
             (folderFeedList.currentState == StateFilter.SOME) ||
             (folderFeedList.currentState == StateFilter.BEST) ) {
            menu.findItem(R.id.menu_search_feeds).setVisible(true);
        } else {
            menu.findItem(R.id.menu_search_feeds).setVisible(false);
        }

        ThemeValue themeValue = PrefsUtils.getSelectedTheme(this);
        if (themeValue == ThemeValue.LIGHT) {
            menu.findItem(R.id.menu_theme_light).setChecked(true);
        } else if (themeValue == ThemeValue.DARK) {
            menu.findItem(R.id.menu_theme_dark).setChecked(true);
        } else if (themeValue == ThemeValue.BLACK) {
            menu.findItem(R.id.menu_theme_black).setChecked(true);
        }

        pm.setOnMenuItemClickListener(this);
        pm.show();
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
		if (item.getItemId() == R.id.menu_refresh) {
            onRefresh();
			return true;
        } else if (item.getItemId() == R.id.menu_search_feeds) {
            if (searchQueryInput.getVisibility() != View.VISIBLE) {
                searchQueryInput.setVisibility(View.VISIBLE);
                searchQueryInput.requestFocus();
            } else {
                searchQueryInput.setText("");
                searchQueryInput.setVisibility(View.GONE);
                checkSearchQuery();
            }
		} else if (item.getItemId() == R.id.menu_add_feed) {
			Intent i = new Intent(this, SearchForFeeds.class);
            startActivity(i);
			return true;
		} else if (item.getItemId() == R.id.menu_logout) {
			DialogFragment newFragment = new LogoutDialogFragment();
			newFragment.show(getSupportFragmentManager(), "dialog");
		} else if (item.getItemId() == R.id.menu_settings) {
            Intent settingsIntent = new Intent(this, Settings.class);
            startActivity(settingsIntent);
            return true;
        } else if (item.getItemId() == R.id.menu_feedback_email) {
            PrefsUtils.sendLogEmail(this);
            return true;
        } else if (item.getItemId() == R.id.menu_feedback_post) {
            try {
                Intent i = new Intent(Intent.ACTION_VIEW);
                i.setData(Uri.parse(PrefsUtils.createFeedbackLink(this)));
                startActivity(i);
            } catch (Exception e) {
                Log.wtf(this.getClass().getName(), "device cannot even open URLs to report feedback");
            }
            return true;
		} else if (item.getItemId() == R.id.menu_textsize) {
			TextSizeDialogFragment textSize = TextSizeDialogFragment.newInstance(PrefsUtils.getListTextSize(this), TextSizeDialogFragment.TextSizeType.ListText);
			textSize.show(getSupportFragmentManager(), TextSizeDialogFragment.class.getName());
			return true;
        } else if (item.getItemId() == R.id.menu_loginas) {
            DialogFragment newFragment = new LoginAsDialogFragment();
            newFragment.show(getSupportFragmentManager(), "dialog");
            return true;
        } else if (item.getItemId() == R.id.menu_theme_light) {
            PrefsUtils.setSelectedTheme(this, ThemeValue.LIGHT);
            UIUtils.restartActivity(this);
        } else if (item.getItemId() == R.id.menu_theme_dark) {
            PrefsUtils.setSelectedTheme(this, ThemeValue.DARK);
            UIUtils.restartActivity(this);
        } else if (item.getItemId() == R.id.menu_theme_black) {
            PrefsUtils.setSelectedTheme(this, ThemeValue.BLACK);
            UIUtils.restartActivity(this);
        }
		return false;
    }

    @OnClick(R.id.main_add_button) void onClickAddButton() {
        Intent i = new Intent(this, SearchForFeeds.class);
        startActivity(i);
    }

    @OnClick(R.id.main_profile_button) void onClickProfileButton() {
        Intent i = new Intent(this, Profile.class);
        startActivity(i);
    }

    @OnClick(R.id.main_user_image) void onClickUserButton() {
        Intent i = new Intent(this, Profile.class);
        startActivity(i);
    }

    @Override
    public void onScrollStateChanged(AbsListView absListView, int i) {
        // not required
    }

    @Override
    public void onScroll(AbsListView view, int firstVisibleItem, int visibleItemCount, int totalItemCount) {
        if (swipeLayout != null) {
            boolean enable = (firstVisibleItem == 0);
            if (wasSwipeEnabled != enable) {
                swipeLayout.setEnabled(enable);
                wasSwipeEnabled = enable;
            }
        }
    }

    // NB: this callback is for the text size slider
	@Override
	public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        float size = AppConstants.LIST_FONT_SIZE[progress];
	    PrefsUtils.setListTextSize(this, size);
        if (folderFeedList != null) folderFeedList.setTextSize(size);
	}

    private void checkSearchQuery() {
        String q = searchQueryInput.getText().toString().trim();
        if (q.length() < 1) {
            q = null;
        }
        folderFeedList.setSearchQuery(q);
    }

    // unused OnSeekBarChangeListener method
	@Override
	public void onStartTrackingTouch(SeekBar seekBar) {
	}

    // unused OnSeekBarChangeListener method
	@Override
	public void onStopTrackingTouch(SeekBar seekBar) {
	}
}

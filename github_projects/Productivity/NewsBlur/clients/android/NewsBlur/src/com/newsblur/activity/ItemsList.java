package com.newsblur.activity;

import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnKeyListener;
import android.widget.EditText;
import android.widget.SeekBar;
import android.widget.SeekBar.OnSeekBarChangeListener;
import android.widget.TextView;

import butterknife.ButterKnife;
import butterknife.Bind;

import com.newsblur.R;
import com.newsblur.fragment.ItemSetFragment;
import com.newsblur.fragment.ReadFilterDialogFragment;
import com.newsblur.fragment.StoryOrderDialogFragment;
import com.newsblur.fragment.TextSizeDialogFragment;
import com.newsblur.service.NBSyncService;
import com.newsblur.util.AppConstants;
import com.newsblur.util.FeedSet;
import com.newsblur.util.FeedUtils;
import com.newsblur.util.PrefConstants.ThemeValue;
import com.newsblur.util.PrefsUtils;
import com.newsblur.util.ReadFilter;
import com.newsblur.util.ReadFilterChangedListener;
import com.newsblur.util.StateFilter;
import com.newsblur.util.StoryListStyle;
import com.newsblur.util.StoryOrder;
import com.newsblur.util.StoryOrderChangedListener;
import com.newsblur.util.UIUtils;

public abstract class ItemsList extends NbActivity implements StoryOrderChangedListener, ReadFilterChangedListener, OnSeekBarChangeListener {

    public static final String EXTRA_FEED_SET = "feed_set";

	private static final String STORY_ORDER = "storyOrder";
	private static final String READ_FILTER = "readFilter";
    private static final String DEFAULT_FEED_VIEW = "defaultFeedView";
    private static final String BUNDLE_ACTIVE_SEARCH_QUERY = "activeSearchQuery";

	protected ItemSetFragment itemSetFragment;
    @Bind(R.id.itemlist_sync_status) TextView overlayStatusText;
    @Bind(R.id.itemlist_search_query) EditText searchQueryInput;
	protected StateFilter intelState;

    protected FeedSet fs;
	
	@Override
    protected void onCreate(Bundle bundle) {
		super.onCreate(bundle);

        overridePendingTransition(R.anim.slide_in_from_right, R.anim.slide_out_to_left);

		fs = (FeedSet) getIntent().getSerializableExtra(EXTRA_FEED_SET);
		intelState = PrefsUtils.getStateFilter(this);

        // this is not strictly necessary, since our first refresh with the fs will swap in
        // the correct session, but that can be delayed by sync backup, so we try here to
        // reduce UI lag, or in case somehow we got redisplayed in a zero-story state
        FeedUtils.prepareReadingSession(fs, false);

        if (PrefsUtils.isAutoOpenFirstUnread(this)) {
            if (FeedUtils.dbHelper.getUnreadCount(fs, intelState) > 0) {
                UIUtils.startReadingActivity(fs, Reading.FIND_FIRST_UNREAD, this);
            }
        }

        getWindow().setBackgroundDrawableResource(android.R.color.transparent);

		setContentView(R.layout.activity_itemslist);
        ButterKnife.bind(this);

		FragmentManager fragmentManager = getSupportFragmentManager();
		itemSetFragment = (ItemSetFragment) fragmentManager.findFragmentByTag(ItemSetFragment.class.getName());
		if (itemSetFragment == null) {
            itemSetFragment = ItemSetFragment.newInstance();
			itemSetFragment.setRetainInstance(true);
			FragmentTransaction transaction = fragmentManager.beginTransaction();
			transaction.add(R.id.activity_itemlist_container, itemSetFragment, ItemSetFragment.class.getName());
			transaction.commit();
		}

        if (bundle != null) {
            String activeSearchQuery = bundle.getString(BUNDLE_ACTIVE_SEARCH_QUERY);
            if (activeSearchQuery != null) {
                searchQueryInput.setText(activeSearchQuery);
                searchQueryInput.setVisibility(View.VISIBLE);
                fs.setSearchQuery(activeSearchQuery);
            }
        }
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
	}

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        if (searchQueryInput != null) {
            String q = searchQueryInput.getText().toString().trim();
            if (q.length() > 0) {
                outState.putString(BUNDLE_ACTIVE_SEARCH_QUERY, q);
            }
        }
    }

    public FeedSet getFeedSet() {
        return this.fs;
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (NBSyncService.isHousekeepingRunning()) finish();
        updateStatusIndicators();
        // Reading activities almost certainly changed the read/unread state of some stories. Ensure
        // we reflect those changes promptly.
        itemSetFragment.hasUpdated();
    }

    @Override
    protected void onPause() {
        super.onPause();
        NBSyncService.addRecountCandidates(fs);
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.itemslist, menu);

        if (fs.isGlobalShared() || 
            fs.isAllSocial() ||
            fs.isFilterSaved() ||
            fs.isAllSaved() ||
            fs.isSingleSavedTag() ||
            fs.isInfrequent() ||
            fs.isAllRead() ) {
            menu.findItem(R.id.menu_mark_all_as_read).setVisible(false);
        }

        if (fs.isGlobalShared() ||
            fs.isAllSocial() ||
            fs.isAllRead() ) {
            menu.findItem(R.id.menu_story_order).setVisible(false);
        }

        if (fs.isGlobalShared() ||
            fs.isFilterSaved() ||
            fs.isAllSaved() ||
            fs.isSingleSavedTag() ||
            fs.isInfrequent() || 
            fs.isAllRead() ) {
            menu.findItem(R.id.menu_read_filter).setVisible(false);
        }

        if (fs.isGlobalShared() ||
            fs.isAllSocial() ||
            fs.isInfrequent() ||
            fs.isAllRead() ) {
            menu.findItem(R.id.menu_search_stories).setVisible(false);
        }

        if ((!fs.isSingleNormal()) || fs.isFilterSaved()) {
            menu.findItem(R.id.menu_notifications).setVisible(false);
            menu.findItem(R.id.menu_delete_feed).setVisible(false);
            menu.findItem(R.id.menu_instafetch_feed).setVisible(false);
            menu.findItem(R.id.menu_intel).setVisible(false);
            menu.findItem(R.id.menu_rename_feed).setVisible(false);
        }

        if (!fs.isInfrequent()) {
            menu.findItem(R.id.menu_infrequent_cutoff).setVisible(false);
        }

		return true;
	}

	@Override
	public boolean onPrepareOptionsMenu(Menu menu) {
		super.onPrepareOptionsMenu(menu);

        StoryListStyle listStyle = PrefsUtils.getStoryListStyle(this, fs);
        if (listStyle == StoryListStyle.GRID_F) {
             menu.findItem(R.id.menu_list_style_grid_f).setChecked(true);
        } else if (listStyle == StoryListStyle.GRID_M) {
             menu.findItem(R.id.menu_list_style_grid_m).setChecked(true);
        } else if (listStyle == StoryListStyle.GRID_C) {
             menu.findItem(R.id.menu_list_style_grid_c).setChecked(true);
        } else {
            menu.findItem(R.id.menu_list_style_list).setChecked(true);
        }

        ThemeValue themeValue = PrefsUtils.getSelectedTheme(this);
        if (themeValue == ThemeValue.LIGHT) {
            menu.findItem(R.id.menu_theme_light).setChecked(true);
        } else if (themeValue == ThemeValue.DARK) {
            menu.findItem(R.id.menu_theme_dark).setChecked(true);
        } else if (themeValue == ThemeValue.BLACK) {
            menu.findItem(R.id.menu_theme_black).setChecked(true);
        }

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		if (item.getItemId() == android.R.id.home) {
			finish();
			return true;
		} else if (item.getItemId() == R.id.menu_mark_all_as_read) {
            FeedUtils.markRead(this, fs, null, null, R.array.mark_all_read_options, true);
			return true;
		} else if (item.getItemId() == R.id.menu_story_order) {
            StoryOrder currentValue = getStoryOrder();
            StoryOrderDialogFragment storyOrder = StoryOrderDialogFragment.newInstance(currentValue);
            storyOrder.show(getSupportFragmentManager(), STORY_ORDER);
            return true;
        } else if (item.getItemId() == R.id.menu_read_filter) {
            ReadFilter currentValue = getReadFilter();
            ReadFilterDialogFragment readFilter = ReadFilterDialogFragment.newInstance(currentValue);
            readFilter.show(getSupportFragmentManager(), READ_FILTER);
            return true;
		} else if (item.getItemId() == R.id.menu_textsize) {
			TextSizeDialogFragment textSize = TextSizeDialogFragment.newInstance(PrefsUtils.getListTextSize(this), TextSizeDialogFragment.TextSizeType.ListText);
			textSize.show(getSupportFragmentManager(), TextSizeDialogFragment.class.getName());
			return true;
        } else if (item.getItemId() == R.id.menu_search_stories) {
            if (searchQueryInput.getVisibility() != View.VISIBLE) {
                searchQueryInput.setVisibility(View.VISIBLE);
                searchQueryInput.requestFocus();
            } else {
                searchQueryInput.setVisibility(View.GONE);
                checkSearchQuery();
            }
        } else if (item.getItemId() == R.id.menu_theme_light) {
            PrefsUtils.setSelectedTheme(this, ThemeValue.LIGHT);
            UIUtils.restartActivity(this);
        } else if (item.getItemId() == R.id.menu_theme_dark) {
            PrefsUtils.setSelectedTheme(this, ThemeValue.DARK);
            UIUtils.restartActivity(this);
        } else if (item.getItemId() == R.id.menu_theme_black) {
            PrefsUtils.setSelectedTheme(this, ThemeValue.BLACK);
            UIUtils.restartActivity(this);
        } else if (item.getItemId() == R.id.menu_list_style_list) {
            PrefsUtils.updateStoryListStyle(this, fs, StoryListStyle.LIST);
            itemSetFragment.updateStyle();
        } else if (item.getItemId() == R.id.menu_list_style_grid_f) {
            PrefsUtils.updateStoryListStyle(this, fs, StoryListStyle.GRID_F);
            itemSetFragment.updateStyle();
        } else if (item.getItemId() == R.id.menu_list_style_grid_m) {
            PrefsUtils.updateStoryListStyle(this, fs, StoryListStyle.GRID_M);
            itemSetFragment.updateStyle();
        } else if (item.getItemId() == R.id.menu_list_style_grid_c) {
            PrefsUtils.updateStoryListStyle(this, fs, StoryListStyle.GRID_C);
            itemSetFragment.updateStyle();
        }
	
		return false;
	}
	
	public StoryOrder getStoryOrder() {
        return PrefsUtils.getStoryOrder(this, fs);
    }
    
	private void updateStoryOrderPreference(StoryOrder newOrder) {
        PrefsUtils.updateStoryOrder(this, fs, newOrder);
    }
	
	private ReadFilter getReadFilter() {
        return PrefsUtils.getReadFilter(this, fs);
    }

    private void updateReadFilterPreference(ReadFilter newValue) {
        PrefsUtils.updateReadFilter(this, fs, newValue);
    }

    @Override
	public void handleUpdate(int updateType) {
        if ((updateType & UPDATE_REBUILD) != 0) {
            finish();
        }
        if ((updateType & UPDATE_STATUS) != 0) {
            updateStatusIndicators();
        }
		if ((updateType & UPDATE_STORY) != 0) {
            if (itemSetFragment != null) {
			    itemSetFragment.hasUpdated();
            }
        }
    }

    private void updateStatusIndicators() {
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

    private void checkSearchQuery() {
        String oldQuery = fs.getSearchQuery();
        String q = searchQueryInput.getText().toString().trim();
        if (q.length() < 1) {
            q = null;
        }
        fs.setSearchQuery(q);
        if (!TextUtils.equals(q, oldQuery)) {
            FeedUtils.prepareReadingSession(fs, true);
            triggerSync();
            itemSetFragment.resetEmptyState();
            itemSetFragment.hasUpdated();
            itemSetFragment.scrollToTop();
        }
    }

	@Override
    public void storyOrderChanged(StoryOrder newValue) {
        updateStoryOrderPreference(newValue);
        restartReadingSession();
    }

    @Override
    public void readFilterChanged(ReadFilter newValue) {
        updateReadFilterPreference(newValue);
        restartReadingSession();
    }

    protected void restartReadingSession() {
        NBSyncService.resetFetchState(fs);
        FeedUtils.prepareReadingSession(fs, true);
        triggerSync();
        itemSetFragment.resetEmptyState();
        itemSetFragment.hasUpdated();
        itemSetFragment.scrollToTop();
    }

    // NB: this callback is for the text size slider
	@Override
	public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        float size = AppConstants.LIST_FONT_SIZE[progress];
	    PrefsUtils.setListTextSize(this, size);
        if (itemSetFragment != null) itemSetFragment.setTextSize(size);
	}

    // unused OnSeekBarChangeListener method
	@Override
	public void onStartTrackingTouch(SeekBar seekBar) {
	}

    // unused OnSeekBarChangeListener method
	@Override
	public void onStopTrackingTouch(SeekBar seekBar) {
	}

    @Override
    public void finish() {
        super.finish();
        /*
         * Animate out the list by sliding it to the right and the Main activity in from
         * the left.  Do this when going back to Main as a subtle hint to the swipe gesture,
         * to make the gesture feel more natural, and to override the really ugly transition
         * used in some of the newer platforms.
         */
        overridePendingTransition(R.anim.slide_in_from_left, R.anim.slide_out_to_right);
    }
}

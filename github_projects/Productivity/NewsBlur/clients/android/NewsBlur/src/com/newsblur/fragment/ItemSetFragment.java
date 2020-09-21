package com.newsblur.fragment;

import android.database.Cursor;
import android.graphics.Typeface;
import android.graphics.Rect;
import android.os.Bundle;
import android.os.Parcelable;
import android.support.v4.app.LoaderManager;
import android.support.v4.content.Loader;
import android.support.v7.widget.GridLayoutManager;
import android.support.v7.widget.RecyclerView;
import android.view.GestureDetector;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import butterknife.ButterKnife;
import butterknife.Bind;

import com.newsblur.R;
import com.newsblur.activity.ItemsList;
import com.newsblur.activity.NbActivity;
import com.newsblur.database.StoryViewAdapter;
import com.newsblur.domain.Story;
import com.newsblur.service.NBSyncService;
import com.newsblur.util.FeedSet;
import com.newsblur.util.FeedUtils;
import com.newsblur.util.PrefsUtils;
import com.newsblur.util.ReadFilter;
import com.newsblur.util.StoryListStyle;
import com.newsblur.util.UIUtils;
import com.newsblur.util.ViewUtils;
import com.newsblur.view.ProgressThrobber;

public class ItemSetFragment extends NbFragment implements LoaderManager.LoaderCallbacks<Cursor> {

	public static int ITEMLIST_LOADER = 0x01;
    private static final String BUNDLE_GRIDSTATE = "gridstate";

    protected boolean cursorSeenYet = false; // have we yet seen a valid cursor for our particular feedset?

    private int itemGridWidthPx = 0;
    private int columnCount;

    private final static int GRID_SPACING_DP = 5;
    private int gridSpacingPx;

	@Bind(R.id.itemgridfragment_grid) RecyclerView itemGrid;
    private GridLayoutManager layoutManager;
    private StoryViewAdapter adapter;
    // an optional pending scroll state to restore.
    private Parcelable gridState;

    // loading indicator for when stories are absent or stale (at top of list)
    @Bind(R.id.top_loading_throb) ProgressThrobber topProgressView;
    // loading indicator for when stories are present and fresh (at bottom of list)
    protected ProgressThrobber bottomProgressView;

    @Bind(R.id.empty_view) View emptyView;
    @Bind(R.id.empty_view_text) TextView emptyViewText;
    @Bind(R.id.empty_view_image) ImageView emptyViewImage;

    private View fleuronFooter;
    // the fleuron has padding that can't be calculated until after layout, but only changes
    // rarely thereafter
    private boolean fleuronResized = false;

    // de-dupe the massive stream of scrolling data to auto-mark read
    private int lastAutoMarkIndex = -1;

    public int indexOfLastUnread = -1;
    public boolean fullFlingComplete = false;

	public static ItemSetFragment newInstance() {
		ItemSetFragment fragment = new ItemSetFragment();
		Bundle arguments = new Bundle();
		fragment.setArguments(arguments);
		return fragment;
	}
    
    @Override
	public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        getLoaderManager().initLoader(ITEMLIST_LOADER, null, this);
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    @Override
    public void onPause() {
        // a pause/resume cycle will depopulate and repopulate the list and trigger bad scroll
        // readings and cause zero-index refreshes, wasting massive cycles. hold the refresh logic
        // until the loaders reset
        cursorSeenYet = false;
        super.onPause();
    }

    @Override
    public void onResume() {
        super.onResume();
        fleuronResized = false;
        updateLoadingIndicators();
    }

    @Override
    public void onViewStateRestored(Bundle savedInstanceState) {
        super.onViewStateRestored(savedInstanceState);
        if (savedInstanceState == null) return;
        gridState = savedInstanceState.getParcelable(BUNDLE_GRIDSTATE);
        // dont actually re-use the state yet, the adapter probably doesn't have any data
        // and won't know how to scroll. swapCursor() will pass this to the adapter when
        // data are ready.
    }

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		View v = inflater.inflate(R.layout.fragment_itemgrid, null);
        ButterKnife.bind(this, v);

        // disable the throbbers if animations are going to have a zero time scale
        boolean isDisableAnimations = ViewUtils.isPowerSaveMode(getActivity());

        topProgressView.setEnabled(!isDisableAnimations);
        topProgressView.setColors(UIUtils.getColor(getActivity(), R.color.refresh_1),
                                  UIUtils.getColor(getActivity(), R.color.refresh_2),
                                  UIUtils.getColor(getActivity(), R.color.refresh_3),
                                  UIUtils.getColor(getActivity(), R.color.refresh_4));

        View footerView = inflater.inflate(R.layout.row_loading_throbber, null);
        bottomProgressView = (ProgressThrobber) footerView.findViewById(R.id.itemlist_loading_throb);
        bottomProgressView.setEnabled(!isDisableAnimations);
        bottomProgressView.setColors(UIUtils.getColor(getActivity(), R.color.refresh_1),
                                     UIUtils.getColor(getActivity(), R.color.refresh_2),
                                     UIUtils.getColor(getActivity(), R.color.refresh_3),
                                     UIUtils.getColor(getActivity(), R.color.refresh_4));

        fleuronFooter = inflater.inflate(R.layout.row_fleuron, null);
        fleuronFooter.setVisibility(View.INVISIBLE);

        itemGrid.getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {
            @Override
            public void onGlobalLayout() {
                itemGridWidthPx = itemGrid.getMeasuredWidth();
                itemGrid.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                updateStyle();
            }
        });
    
        StoryListStyle listStyle = PrefsUtils.getStoryListStyle(getActivity(), getFeedSet());

        calcColumnCount(listStyle);
        layoutManager = new GridLayoutManager(getActivity(), columnCount);
        itemGrid.setLayoutManager(layoutManager);
        setupAnimSpeeds();

        calcGridSpacing(listStyle);
        itemGrid.addItemDecoration(new RecyclerView.ItemDecoration() {
            @Override
            public void getItemOffsets(Rect outRect, View view, RecyclerView parent, RecyclerView.State state) {
                outRect.set(gridSpacingPx, gridSpacingPx, gridSpacingPx, gridSpacingPx);
            }
        });

        adapter = new StoryViewAdapter(((NbActivity) getActivity()), this, getFeedSet(), listStyle);
        adapter.addFooterView(footerView);
        adapter.addFooterView(fleuronFooter);
        itemGrid.setAdapter(adapter); 

        // the layout manager needs to know that the footer rows span all the way across
        layoutManager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(int position) {
                switch (adapter.getItemViewType(position)) {
                    case StoryViewAdapter.VIEW_TYPE_FOOTER:
                        return columnCount;
                    default:
                        return 1;
                }
            }
        });

        itemGrid.addOnScrollListener(new RecyclerView.OnScrollListener() {
            @Override
            public void onScrolled(RecyclerView recyclerView, int dx, int dy) {
                ItemSetFragment.this.onScrolled(recyclerView, dx, dy);
            }
        });

        setupGestureDetector(itemGrid);

		return v;
	}

    protected void triggerRefresh(int desiredStoryCount, Integer totalSeen) {
        if (getFeedSet().isMuted()) return;

        // ask the sync service for as many stories as we want
        boolean gotSome = NBSyncService.requestMoreForFeed(getFeedSet(), desiredStoryCount, totalSeen);
        // if the service thinks it can get more, or if we haven't even seen a cursor yet, start the service
        if (gotSome || (totalSeen == null)) triggerSync();
    }

    /**
     * Indicate that the DB was cleared.
     */
    public void resetEmptyState() {
        updateAdapter(null);
        cursorSeenYet = false;
    }

    /**
     * A calback for our adapter that async thaws the story list so the fragment can have
     * some info about the story list when it is ready.
     */
    public void storyThawCompleted(int indexOfLastUnread) {
        this.indexOfLastUnread = indexOfLastUnread;
        this.fullFlingComplete = false;
        // we don't actually calculate list speed until it has some stories
        setupAnimSpeeds();
    }

    public void scrollToTop() {
        layoutManager.scrollToPositionWithOffset(0, 0);
    }

    protected FeedSet getFeedSet() {
        return ((ItemsList) getActivity()).getFeedSet();
    }

	public void hasUpdated() {
        if (isAdded() && !getFeedSet().isMuted()) {
		    getLoaderManager().restartLoader(ITEMLIST_LOADER , null, this);
        }
	}

	@Override
	public Loader<Cursor> onCreateLoader(int arg0, Bundle arg1) {
        FeedSet fs = getFeedSet();
        if (fs == null) {
            com.newsblur.util.Log.e(this.getClass().getName(), "can't create fragment, no feedset ready");
            // this is probably happening in a finalisation cycle or during a crash, pop the activity stack
            try {
                getActivity().finish();
            } catch (Exception e) {
                ;
            }
            return FeedUtils.dbHelper.getNullLoader();
        } else if (fs.isMuted()) {
            return FeedUtils.dbHelper.getNullLoader();
        } else {
            return FeedUtils.dbHelper.getActiveStoriesLoader(getFeedSet());
        }
    }

    @Override
	public synchronized void onLoadFinished(Loader<Cursor> loader, Cursor cursor) {
		if (cursor != null) {
            if (! FeedUtils.dbHelper.isFeedSetReady(getFeedSet())) {
                // the DB hasn't caught up yet from the last story list; don't display stale stories.
                com.newsblur.util.Log.i(this.getClass().getName(), "stale load");
                updateAdapter(null);
                triggerRefresh(1, null);
            } else {
                cursorSeenYet = true;
                com.newsblur.util.Log.d(this.getClass().getName(), "loaded cursor with count: " + cursor.getCount());
                updateAdapter(cursor);
                if (cursor.getCount() < 1) {
                    triggerRefresh(1, 0);
                }
            }
		}
        updateLoadingIndicators();
	}

    protected void updateAdapter(Cursor cursor) {
        adapter.swapCursor(cursor, itemGrid, gridState);
        gridState = null;
        adapter.updateFeedSet(getFeedSet());
        if ((cursor != null) && (cursor.getCount() > 0)) {
            emptyView.setVisibility(View.INVISIBLE);
        } else {
            emptyView.setVisibility(View.VISIBLE);
        }

        // though we have stories, we might not yet have as many as we want
        ensureSufficientStories();
    }

	@Override
	public void onLoaderReset(Loader<Cursor> loader) {
	}

    private void updateLoadingIndicators() {
        // sanity check that we even have views yet
        if (fleuronFooter == null) return;

        calcFleuronPadding();

        if (getFeedSet().isMuted()) {
            emptyViewText.setText(R.string.empty_list_view_muted_feed);
            emptyViewText.setTypeface(null, Typeface.NORMAL);
            emptyViewImage.setVisibility(View.VISIBLE);
            topProgressView.setVisibility(View.INVISIBLE);
            bottomProgressView.setVisibility(View.INVISIBLE);
            return;
        }

        if ( (!cursorSeenYet) || NBSyncService.isFeedSetSyncing(getFeedSet(), getActivity()) ) {
            emptyViewText.setText(R.string.empty_list_view_loading);
            emptyViewText.setTypeface(null, Typeface.ITALIC);
            emptyViewImage.setVisibility(View.INVISIBLE);

            if (NBSyncService.isFeedSetStoriesFresh(getFeedSet())) {
                topProgressView.setVisibility(View.INVISIBLE);
                bottomProgressView.setVisibility(View.VISIBLE);
            } else {
                topProgressView.setVisibility(View.VISIBLE);
                bottomProgressView.setVisibility(View.GONE);
            }
            fleuronFooter.setVisibility(View.INVISIBLE);
        } else {
            ReadFilter readFilter = PrefsUtils.getReadFilter(getActivity(), getFeedSet());
            if (readFilter == ReadFilter.UNREAD) {
                emptyViewText.setText(R.string.empty_list_view_no_stories_unread);
            } else {
                emptyViewText.setText(R.string.empty_list_view_no_stories);
            }
            emptyViewText.setTypeface(null, Typeface.NORMAL);
            emptyViewImage.setVisibility(View.VISIBLE);

            topProgressView.setVisibility(View.INVISIBLE);
            bottomProgressView.setVisibility(View.INVISIBLE);
            if (cursorSeenYet && NBSyncService.isFeedSetExhausted(getFeedSet()) && (adapter.getRawStoryCount() > 0)) {
                fleuronFooter.setVisibility(View.VISIBLE);
            }
        }
    }

    public void updateStyle() {
        StoryListStyle listStyle = PrefsUtils.getStoryListStyle(getActivity(), getFeedSet());
        calcColumnCount(listStyle);
        calcGridSpacing(listStyle);
        layoutManager.setSpanCount(columnCount);
        adapter.setStyle(listStyle);
        adapter.notifyAllItemsChanged();
    }

    public void setTextSize(Float size) {
        if (adapter != null) {
            adapter.setTextSize(size);
            adapter.notifyAllItemsChanged();
        }
    }

    private void calcColumnCount(StoryListStyle listStyle) {
        // sensible defaults
        int colsFine = 3;
        int colsMed = 2;
        int colsCoarse = 1;

        // ensure we have measured
        if (itemGridWidthPx > 0) {
            int itemGridWidthDp = Math.round(UIUtils.px2dp(getActivity(), itemGridWidthPx));
            colsCoarse = itemGridWidthDp / 300;
            colsMed = itemGridWidthDp / 200;
            colsFine = itemGridWidthDp / 150;
            // sanity check the counts are strictly increasing
            if (colsCoarse < 1) colsCoarse = 1;
            if (colsMed <= colsCoarse) colsMed = colsCoarse + 1;
            if (colsFine <= colsMed) colsFine = colsMed +1;
        }

        if (listStyle == StoryListStyle.GRID_F) {
            columnCount = colsFine;
        } else if (listStyle == StoryListStyle.GRID_M) {
            columnCount = colsMed;
        } else if (listStyle == StoryListStyle.GRID_C) {
            columnCount = colsCoarse;
        } else {
            columnCount = 1;
        }
    }

    private void calcGridSpacing(StoryListStyle listStyle) {
        if (listStyle == StoryListStyle.LIST) {
            gridSpacingPx = 0;
        } else {
            gridSpacingPx = UIUtils.dp2px(getActivity(), GRID_SPACING_DP);
        }
    }

    private void setupAnimSpeeds() {
        // to mitigate taps missed because of list pushdowns, RVs animate them. however, the speed
        // is device and settings dependent.  to keep the UI consistent across installs, take the
        // system default speed and tweak it towards a speed that looked and functioned well in
        // testing while still somewhat respecting the system's requested adjustments to speed.
        long targetAddDuration = 250L;
        // moves are especially jarring, and very rare
        long targetMovDuration = 500L;
        // if there are no stories in the list at all, let the first insert happen very quickly
        if ((adapter == null) || (adapter.getRawStoryCount() < 1)) {
            targetAddDuration = 0L;
            targetMovDuration = 0L;
        }

        RecyclerView.ItemAnimator anim = itemGrid.getItemAnimator();
        anim.setAddDuration((long) ((anim.getAddDuration() + targetAddDuration)/2L));
        anim.setMoveDuration((long) ((anim.getMoveDuration() + targetMovDuration)/2L));
    }

    private void onScrolled(RecyclerView recyclerView, int dx, int dy) {
        // the framework likes to trigger this on init before we even known counts, so disregard those
        if (!cursorSeenYet) return;

        // don't bother checking on scroll up
        if (dy < 1) return;

        ensureSufficientStories();

        // the list can be scrolled past the last item thanks to the offset footer, but don't fling
        // past the last item, which can be confusing to users who don't know about or need the offset
        if ( (!fullFlingComplete) &&
             (layoutManager.findLastCompletelyVisibleItemPosition() >= adapter.getStoryCount()) ) {
            itemGrid.stopScroll();
            // but after halting at the end once, do allow scrolling past the bottom
            fullFlingComplete = true;
        }

        // if flinging downwards, pause at the last unread as a convenience
        if ( (indexOfLastUnread >= 0) &&
             (layoutManager.findLastCompletelyVisibleItemPosition() >= indexOfLastUnread) ) {
            // but don't interrupt if already past the last unread
            if (indexOfLastUnread >= layoutManager.findFirstCompletelyVisibleItemPosition()) {
                itemGrid.stopScroll();
            }
            indexOfLastUnread = -1;
        }

        if (PrefsUtils.isMarkReadOnScroll(getActivity())) {
            // we want the top row of stories that is partially obscured. go back one from the first fully visible
            int markEnd = layoutManager.findFirstCompletelyVisibleItemPosition() - 1;
            if (markEnd > lastAutoMarkIndex) {
                lastAutoMarkIndex = markEnd;
                // iterate backwards through that row, marking read
                for (int i=0; i<columnCount; i++) {
                    int index = markEnd - i;
                    Story story = adapter.getStory(index);
                    if (story != null) {
                        FeedUtils.markStoryAsRead(story, getActivity());
                    }
                }
            }
        }
    }

    private void ensureSufficientStories() {
        // don't ask the list for how many rows it actually has - it may still be thawing from the cursor
        int totalCount = adapter.getRawStoryCount();
        int visibleCount = layoutManager.getChildCount();
        int lastVisible = layoutManager.findLastVisibleItemPosition();
        
        // load an extra page worth of stories past the viewport plus at least two rows to prime the height calc
        int desiredStoryCount = lastVisible + (visibleCount*2) + (columnCount*2);
        triggerRefresh(desiredStoryCount, totalCount);
        //com.newsblur.util.Log.d(this, String.format(" total:%d  bound:%d  last%d  desire:%d", totalCount, visibleCount, lastVisible, desiredStoryCount));
    }

    private void setupGestureDetector(RecyclerView v) {
        final GestureDetector gestureDetector = new GestureDetector(getActivity(), new SwipeBackGestureDetector());
        v.addOnItemTouchListener(new RecyclerView.SimpleOnItemTouchListener() {
            public boolean onInterceptTouchEvent(RecyclerView rv, MotionEvent e) {
                return gestureDetector.onTouchEvent(e);
            }
        });
    }

    /**
     * A detector for the standard "swipe back out of activity" Android gesture.  Note that this does
     * not necessarily wait for an UP event, as RecyclerViews like to capture them.
     */
    class SwipeBackGestureDetector extends GestureDetector.SimpleOnGestureListener {
        @Override
        public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX, float distanceY) {
            if (e1 == null) return false;
            if ((e1.getX() < 60f) &&                  // the gesture should start from the left bezel and
                ((e2.getX()-e1.getX()) > 90f) &&      // move horizontally to the right and
                (Math.abs(e1.getY()-e2.getY()) < 40f) // have minimal vertical travel, so we don't capture scrolling gestures
                ) {
                ItemSetFragment.this.getActivity().finish();
                return true;
            }
            return false;
        }
    }

    /**
     * if the story list bottom has been reached, add an amount of padding to the footer so that it can still
     * be scrolled until the bottom most story reaches to top, for those who mark-by-scrolling.
     */
    private void calcFleuronPadding() {
        if (fleuronResized) return;
        int listHeight = itemGrid.getMeasuredHeight();
        View innerView = fleuronFooter.findViewById(R.id.fleuron);
        ViewGroup.LayoutParams oldLayout = innerView.getLayoutParams();
        ViewGroup.MarginLayoutParams newLayout = new LinearLayout.LayoutParams(oldLayout);
        int marginPx_4dp = UIUtils.dp2px(getActivity(), 4);
        int defaultPx_100dp = UIUtils.dp2px(getActivity(), 100);
        int bufferPx_50dp = UIUtils.dp2px(getActivity(), 50);
        if (listHeight > 1) {
            newLayout.setMargins(0, marginPx_4dp, 0, listHeight-bufferPx_50dp);
            fleuronResized = true;
        } else {
            newLayout.setMargins(0, marginPx_4dp, 0, defaultPx_100dp);
        }
        innerView.setLayoutParams(newLayout);
    }

    @Override
    public void onSaveInstanceState (Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putParcelable(BUNDLE_GRIDSTATE, itemGrid.getLayoutManager().onSaveInstanceState());
    }

}

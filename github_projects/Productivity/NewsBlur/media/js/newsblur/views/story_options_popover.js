NEWSBLUR.StoryOptionsPopover = NEWSBLUR.ReaderPopover.extend({
    
    className: "NB-style-popover",
    
    options: {
        'width': 264,
        'anchor': '.NB-taskbar-options',
        'placement': 'top right',
        'offset': {
            top: 12,
            left: -64
        },
        'overlay_bottom': true,
        'popover_class': 'NB-style-popover-container'
    },
    
    events: {
        "click .NB-font-family-option": "change_font_family",
        "click .NB-story-font-size-option": "change_story_font_size",
        "click .NB-feed-font-size-option": "change_feed_font_size",
        "click .NB-line-spacing-option": "change_line_spacing",
        "click .NB-story-titles-pane-option": "change_story_titles_pane",
        "click .NB-single-story-option": "change_single_story",
        "click .NB-grid-columns-option": "change_grid_columns",
        "click .NB-premium-link": "open_premium_modal"
    },
    
    initialize: function(options) {
        this.options = _.extend({}, this.options, options);
        NEWSBLUR.ReaderPopover.prototype.initialize.call(this, this.options);
        this.model = NEWSBLUR.assets;
        this.render();
        this.show_correct_options();
    },
    
    close: function() {
        NEWSBLUR.reader.$s.$taskbar_options.removeClass('NB-active');
        NEWSBLUR.ReaderPopover.prototype.close.apply(this, arguments);
    },

    render: function() {
        var self = this;
        var feed = NEWSBLUR.assets.active_feed;
        
        NEWSBLUR.ReaderPopover.prototype.render.call(this);
        
        this.$el.html($.make('div', [
            $.make('div', { className: 'NB-popover-section' }, [
                $.make('div', { className: 'NB-popover-section-title' }, 'Story Layout - Split'),
                $.make('ul', { className: 'segmented-control NB-options-story-titles-pane' }, [
                    $.make('li', { className: 'NB-story-titles-pane-option NB-options-story-titles-pane-north' }, [
                        $.make('div', { className: 'NB-icon' }),
                        'Top'
                    ]),
                    $.make('li', { className: 'NB-story-titles-pane-option NB-options-story-titles-pane-west' }, [
                        $.make('div', { className: 'NB-icon' }),
                        'Left'
                    ]),
                    $.make('li', { className: 'NB-story-titles-pane-option NB-options-story-titles-pane-south NB-active' }, [
                        $.make('div', { className: 'NB-icon' }),
                        'Bottom'
                    ])
                ]),
                $.make('ul', { className: 'segmented-control NB-options-single-story' }, [
                    $.make('li', { className: 'NB-single-story-option NB-options-single-story-off NB-active' }, [
                        $.make('div', { className: 'NB-icon' }),
                        'All Stories'
                    ]),
                    $.make('li', { className: 'NB-single-story-option NB-options-single-story-on' }, [
                        $.make('div', { className: 'NB-icon' }),
                        'Single Story'
                    ])
                ])

            ]),
            $.make('div', { className: 'NB-popover-section' }, [
                $.make('div', { className: 'NB-popover-section-title' }, 'Story Layout - Grid Columns'),
                $.make('ul', { className: 'segmented-control NB-options-grid-columns' }, [
                    $.make('li', { className: 'NB-grid-columns-option NB-options-grid-columns-0' }, [
                        $.make('div', { className: 'NB-icon' }),
                        'Auto'
                    ]),
                    $.make('li', { className: 'NB-grid-columns-option NB-options-grid-columns-1' }, [
                        '1'
                    ]),
                    $.make('li', { className: 'NB-grid-columns-option NB-options-grid-columns-2' }, [
                        '2'
                    ]),
                    $.make('li', { className: 'NB-grid-columns-option NB-options-grid-columns-3' }, [
                        '3'
                    ]),
                    $.make('li', { className: 'NB-grid-columns-option NB-options-grid-columns-4' }, [
                        '4'
                    ])
                ])
            ]),
            $.make('div', { className: 'NB-popover-section' }, [
                $.make('div', { className: 'NB-popover-section-title' }, 'Font Family'),
                $.make('ul', { className: 'segmented-control-vertical NB-options-font-family' }, [
                    $.make('li', { className: 'NB-font-family-option NB-options-font-family-sans-serif NB-active' }, 'Helvetica'),
                    $.make('li', { className: 'NB-font-family-option NB-options-font-family-serif' }, 'Palatino / Georgia'),
                    $.make('li', { className: 'NB-font-family-option NB-premium-only NB-options-font-family-gotham' }, [
                        (!NEWSBLUR.Globals.is_premium && $.make('div', { className: 'NB-tag' }, 'Premium')),
                        'Gotham Narrow'
                    ]),
                    $.make('li', { className: 'NB-font-family-option NB-premium-only NB-options-font-family-sentinel' }, [
                        (!NEWSBLUR.Globals.is_premium && $.make('div', { className: 'NB-tag' }, 'Premium')),
                        'Sentinel'
                    ]),
                    $.make('li', { className: 'NB-font-family-option NB-premium-only NB-options-font-family-whitney' }, [
                        (!NEWSBLUR.Globals.is_premium && $.make('div', { className: 'NB-tag' }, 'Premium')),
                        'Whitney'
                    ]),
                    $.make('li', { className: 'NB-font-family-option NB-premium-only NB-options-font-family-chronicle' }, [
                        (!NEWSBLUR.Globals.is_premium && $.make('div', { className: 'NB-tag' }, 'Premium')),
                        'Chronicle'
                    ])
                ]),
                (!NEWSBLUR.Globals.is_premium && $.make('div', { className: 'NB-premium-explainer' }, [
                    'Premium fonts require a ',
                    $.make('spam', { className: 'NB-splash-link NB-premium-link' }, 'premium account')
                ]))
            ]),
            $.make('div', { className: 'NB-popover-section' }, [
                $.make('div', { className: 'NB-popover-section-title' }, 'Font Size - Stories'),
                $.make('ul', { className: 'segmented-control NB-options-story-font-size' }, [
                    $.make('li', { className: 'NB-story-font-size-option NB-options-font-size-xs' }, 'XS'),
                    $.make('li', { className: 'NB-story-font-size-option NB-options-font-size-s' }, 'S'),
                    $.make('li', { className: 'NB-story-font-size-option NB-options-font-size-m NB-active' }, 'M'),
                    $.make('li', { className: 'NB-story-font-size-option NB-options-font-size-l' }, 'L'),
                    $.make('li', { className: 'NB-story-font-size-option NB-options-font-size-xl' }, 'XL')
                ]),
                $.make('ul', { className: 'segmented-control NB-options-line-spacing' }, [
                    $.make('li', { className: 'NB-line-spacing-option NB-options-line-spacing-xs' }, $.make('div', { className: 'NB-icon' })),
                    $.make('li', { className: 'NB-line-spacing-option NB-options-line-spacing-s' }, $.make('div', { className: 'NB-icon' })),
                    $.make('li', { className: 'NB-line-spacing-option NB-options-line-spacing-m NB-active' }, $.make('div', { className: 'NB-icon' })),
                    $.make('li', { className: 'NB-line-spacing-option NB-options-line-spacing-l' }, $.make('div', { className: 'NB-icon' })),
                    $.make('li', { className: 'NB-line-spacing-option NB-options-line-spacing-xl' }, $.make('div', { className: 'NB-icon' }))
                ]),
                $.make('div', { className: 'NB-popover-section-title' }, 'Font Size - Feeds'),
                $.make('ul', { className: 'segmented-control NB-options-feed-font-size' }, [
                    $.make('li', { className: 'NB-feed-font-size-option NB-options-font-size-xs' }, 'XS'),
                    $.make('li', { className: 'NB-feed-font-size-option NB-options-font-size-s' }, 'S'),
                    $.make('li', { className: 'NB-feed-font-size-option NB-options-font-size-m NB-active' }, 'M'),
                    $.make('li', { className: 'NB-feed-font-size-option NB-options-font-size-l' }, 'L'),
                    $.make('li', { className: 'NB-feed-font-size-option NB-options-font-size-xl' }, 'XL')
                ])
            ])
        ]));
        
        return this;
    },
    
    show_correct_options: function() {
        var font_family = NEWSBLUR.assets.preference('story_styling');
        var story_font_size = NEWSBLUR.assets.preference('story_size');
        var feed_font_size = NEWSBLUR.assets.preference('feed_size');
        var line_spacing = NEWSBLUR.assets.preference('story_line_spacing');
        var titles_layout_pane = NEWSBLUR.assets.preference('story_pane_anchor');
        var single_story = NEWSBLUR.assets.preference('feed_view_single_story');
        var grid_columns = NEWSBLUR.assets.preference('grid_columns');
        
        this.$('.NB-font-family-option').removeClass('NB-active');
        this.$('.NB-options-font-family-'+font_family).addClass('NB-active');

        this.$('.NB-feed-font-size-option').removeClass('NB-active');
        this.$('.NB-story-font-size-option').removeClass('NB-active');
        this.$('.NB-options-story-font-size .NB-options-font-size-'+story_font_size).addClass('NB-active');
        this.$('.NB-options-feed-font-size .NB-options-font-size-'+feed_font_size).addClass('NB-active');
        this.$('.NB-line-spacing-option').removeClass('NB-active');
        this.$('.NB-options-line-spacing-'+line_spacing).addClass('NB-active');

        this.$('.NB-story-titles-pane-option').removeClass('NB-active');
        this.$('.NB-options-story-titles-pane-'+titles_layout_pane).addClass('NB-active');
        this.$('.NB-single-story-option').removeClass('NB-active');
        this.$('.NB-options-single-story-'+(single_story?'on':'off')).addClass('NB-active');

        this.$('.NB-grid-columns-option').removeClass('NB-active');
        this.$('.NB-options-grid-columns-'+grid_columns).addClass('NB-active');

        NEWSBLUR.reader.$s.$taskbar_options.addClass('NB-active');
        
        if (!NEWSBLUR.Globals.is_premium) {
            this.$(".NB-premium-only").addClass('NB-disabled').attr('disabled', 'disabled');
        }
    },

    
    // ==========
    // = Events =
    // ==========
    
    change_font_family: function(e) {
        var $target = $(e.target);
        
        if ($target.hasClass("NB-options-font-family-serif")) {
            this.update_font_family('serif');
        } else if ($target.hasClass("NB-options-font-family-sans-serif")) {
            this.update_font_family('sans-serif');
        } else if (NEWSBLUR.Globals.is_premium) {
            if ($target.hasClass("NB-options-font-family-gotham")) {
                this.update_font_family('gotham');
            } else if ($target.hasClass("NB-options-font-family-sentinel")) {
                this.update_font_family('sentinel');
            } else if ($target.hasClass("NB-options-font-family-whitney")) {
                this.update_font_family('whitney');
            } else if ($target.hasClass("NB-options-font-family-chronicle")) {
                this.update_font_family('chronicle');
            }
        }
        
        this.show_correct_options();
    },
    
    update_font_family: function(setting) {
        NEWSBLUR.assets.preference('story_styling', setting);
        NEWSBLUR.reader.apply_story_styling();
    },
    
    change_story_font_size: function(e) {
        var $target = $(e.target);
        
        if ($target.hasClass("NB-options-font-size-xs")) {
            this.update_story_font_size('xs');
        } else if ($target.hasClass("NB-options-font-size-s")) {
            this.update_story_font_size('s');
        } else if ($target.hasClass("NB-options-font-size-m")) {
            this.update_story_font_size('m');
        } else if ($target.hasClass("NB-options-font-size-l")) {
            this.update_story_font_size('l');
        } else if ($target.hasClass("NB-options-font-size-xl")) {
            this.update_story_font_size('xl');
        }
        
        this.show_correct_options();
    },
    
    change_feed_font_size: function(e) {
        var $target = $(e.target);
        
        if ($target.hasClass("NB-options-font-size-xs")) {
            this.update_feed_font_size('xs');
        } else if ($target.hasClass("NB-options-font-size-s")) {
            this.update_feed_font_size('s');
        } else if ($target.hasClass("NB-options-font-size-m")) {
            this.update_feed_font_size('m');
        } else if ($target.hasClass("NB-options-font-size-l")) {
            this.update_feed_font_size('l');
        } else if ($target.hasClass("NB-options-font-size-xl")) {
            this.update_feed_font_size('xl');
        }
        
        this.show_correct_options();
    },
    
    update_story_font_size: function(setting) {
        NEWSBLUR.assets.preference('story_size', setting);
        NEWSBLUR.reader.apply_story_styling();
    },
    
    update_feed_font_size: function(setting) {
        NEWSBLUR.assets.preference('feed_size', setting);
        NEWSBLUR.reader.apply_story_styling();
    },
    
    change_line_spacing: function(e) {
        var $target = $(e.currentTarget);
        
        if ($target.hasClass("NB-options-line-spacing-xs")) {
            this.update_line_spacing('xs');
        } else if ($target.hasClass("NB-options-line-spacing-s")) {
            this.update_line_spacing('s');
        } else if ($target.hasClass("NB-options-line-spacing-m")) {
            this.update_line_spacing('m');
        } else if ($target.hasClass("NB-options-line-spacing-l")) {
            this.update_line_spacing('l');
        } else if ($target.hasClass("NB-options-line-spacing-xl")) {
            this.update_line_spacing('xl');
        }
        
        this.show_correct_options();
    },
    
    update_line_spacing: function(setting) {
        NEWSBLUR.assets.preference('story_line_spacing', setting);
        NEWSBLUR.reader.apply_story_styling();
    },
    
    change_story_titles_pane: function(e) {
        var $target = $(e.currentTarget);
        
        if ($target.hasClass("NB-options-story-titles-pane-north")) {
            this.update_story_titles_pane('north');
        } else if ($target.hasClass("NB-options-story-titles-pane-west")) {
            this.update_story_titles_pane('west');
        } else if ($target.hasClass("NB-options-story-titles-pane-south")) {
            this.update_story_titles_pane('south');
        }
        
        this.show_correct_options();
    },
    
    update_story_titles_pane: function(setting) {
        var old_anchor = NEWSBLUR.assets.preference('story_pane_anchor');
        var pane_size = NEWSBLUR.assets.preference('story_titles_pane_size');
        
        if (setting == 'west' && _.contains(['north', 'south'], old_anchor)) {
            // Moving from top to side
            pane_size *= 2;
        } else if (_.contains(['north', 'south'], setting) && old_anchor == 'west') {
            // Moving from side to top
            pane_size /= 2;
        }
        NEWSBLUR.assets.preference('story_pane_anchor', setting);
        NEWSBLUR.assets.preference('story_titles_pane_size', pane_size);
        NEWSBLUR.reader.apply_resizable_layout({right_side: true});
    },
    
    change_single_story: function(e) {
        var $target = $(e.currentTarget);
        
        if ($target.hasClass("NB-options-single-story-off")) {
            this.update_single_story(0);
        } else if ($target.hasClass("NB-options-single-story-on")) {
            this.update_single_story(1);
        }
        
        this.show_correct_options();
    },
    
    update_single_story: function(setting) {
        NEWSBLUR.assets.preference('feed_view_single_story', setting);
        NEWSBLUR.app.story_list.render();
        _.defer(function() {
            NEWSBLUR.reader.resize_window();
            if (NEWSBLUR.reader.active_story) {
                NEWSBLUR.reader.active_story.set('selected', false).set('selected', true);
            }
        });
    },
    
    change_grid_columns: function(e) {
        var $target = $(e.currentTarget);
        
        if ($target.hasClass("NB-options-grid-columns-0")) {
            this.update_grid_columns(0);
        } else if ($target.hasClass("NB-options-grid-columns-1")) {
            this.update_grid_columns(1);
        } else if ($target.hasClass("NB-options-grid-columns-2")) {
            this.update_grid_columns(2);
        } else if ($target.hasClass("NB-options-grid-columns-3")) {
            this.update_grid_columns(3);
        } else if ($target.hasClass("NB-options-grid-columns-4")) {
            this.update_grid_columns(4);
        }
        
        this.show_correct_options();
    },
    
    update_grid_columns: function(setting) {
        NEWSBLUR.assets.preference('grid_columns', setting);
        NEWSBLUR.app.story_list.render();
        _.defer(function() {
            NEWSBLUR.app.story_titles.override_grid();
            NEWSBLUR.reader.resize_window();
        });
    },
    
    open_premium_modal: function(e) {
      this.close(e, function() {
          NEWSBLUR.reader.open_feedchooser_modal({'premium_only': true});
      });
    }
    
    
});
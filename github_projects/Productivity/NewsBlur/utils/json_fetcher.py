import datetime
import dateutil.parser
from django.conf import settings
from django.utils import feedgenerator
from utils import log as logging
from utils.json_functions import decode

class JSONFetcher:
    
    def __init__(self, feed, options=None):
        self.feed = feed
        self.options = options or {}
    
    def fetch(self, address, raw_feed):
        if not address:
            address = self.feed.feed_address
        
        json_feed = decode(raw_feed.content)
        if not json_feed:
            logging.debug(u'   ***> [%-30s] ~FRJSON fetch failed: %s' % 
                          (self.feed.log_title[:30], address))
            return
        
        data = {}
        data['title'] = json_feed.get('title', '[Untitled]')
        data['link'] = json_feed.get('home_page_url', None)
        data['description'] = json_feed.get('title', "")
        data['lastBuildDate'] = datetime.datetime.utcnow()
        data['generator'] = 'NewsBlur JSON Feed - %s' % settings.NEWSBLUR_URL
        data['docs'] = None
        data['feed_url'] = json_feed.get('feed_url')
        
        rss = feedgenerator.Atom1Feed(**data)

        for item in json_feed.get('items', []):
            story_data = self.json_feed_story(item)
            rss.add_item(**story_data)
        
        return rss.writeString('utf-8')
    
    def json_feed_story(self, item):
        date_published = datetime.datetime.now()
        pubdate = item.get('date_published', None)
        if pubdate:
            date_published = dateutil.parser.parse(pubdate)
        story = {
            'title': item.get('title', None),
            'link': item.get('external_url', item.get('url', None)),
            'description': item.get('content_html', item.get('content_text', None)),
            'author_name': item.get('author', {}).get('name', None),
            'categories': item.get('tags', []),
            'unique_id': unicode(item.get('id', item.get('url', None))),
            'pubdate': date_published,
        }
        
        return story
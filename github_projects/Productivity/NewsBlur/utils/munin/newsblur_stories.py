#!/srv/newsblur/venv/newsblur/bin/python
from utils.munin.base import MuninGraph


class NBMuninGraph(MuninGraph):

    @property
    def graph_config(self):
        return {
            'graph_category' : 'NewsBlur',
            'graph_title' : 'NewsBlur Stories',
            'graph_vlabel' : 'Stories',
            'graph_args' : '-l 0',
            'stories.label': 'Stories',
            'starred_stories.label': 'Starred stories',
        }

    def calculate_metrics(self):
        from apps.rss_feeds.models import MStory, MStarredStory

        return {
            'stories': MStory.objects.count(),
            'starred_stories': MStarredStory.objects.count(),
        }

if __name__ == '__main__':
    NBMuninGraph().run()

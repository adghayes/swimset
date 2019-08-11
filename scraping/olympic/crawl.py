import scrapy
from scrapy.spiders import SitemapSpider
import scrape # content parsing in separate file

class OlympicCrawl(SitemapSpider):
    """Spider for crawling to event results pages of Olympics website"""
    name = 'olympic_crawl'
    sitemap_urls = ['https://www.olympic.org/sitemap.xml']
    sitemap_follow = ['/OlympicGamesEvent/OlympicGamesEvent']

    def __init__(self, games=None , sport=None, *args, **kwargs):
        super(OlympicCrawl, self).__init__(*args, **kwargs)
        event_criteria = '' 
        if games:
            event_criteria += ('/' + games)
        if sport:
            event_criteria += ('/' + sport)
        self.event_criteria = event_criteria 
        self.download_delay = 1

    def sitemap_filter(self, entries):
        for entry in entries:
            if self.event_criteria in entry['loc']:
                if ('language' in entry and entry['language'] == 'en'):
                    yield entry
            elif 'sitemap' in entry['loc']:
                yield entry

    def parse (self, response):
        event_data = scrape.event(response.text)
        event_data.setdefault('url',response.url)
        yield event_data




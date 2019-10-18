import scrapy
from scrapy.spiders import SitemapSpider
import re

class OlympicCrawl(SitemapSpider):
    """Spider for crawling to and scraping event results pages of IOC website"""
    name = 'iocspider'
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
            is_event_sitemap = '/sitemap/OlympicGamesEvent/OlympicGamesEvent' in entry['loc']
            is_target = self.event_criteria in entry['loc']
            is_english = 'language' in entry and entry['language'] == 'en'
            if is_event_sitemap or (is_target and is_english):
                yield entry

    def parse (self, response):
        event = {'url': response.url, "heats": []}
        for section in response.selector.css('.table-box'):
            heat_name = section.css("h2::text").get()
            heat = {'name': heat_name, 'results': []}
            for row in section.css("tr:not(.slide):not(.hide):not(.th-row)"):
                result = {}

                rank = ''.join(row.css("td.col1 *::text").getall()).strip("\. \n\r")
                if rank:
                    result['rank'] = rank

                result['name'] = row.css("td.col2 .name::text").get()

                link = row.css("td.col2 a::attr(href)")
                if link.get():
                    result['link'] = link.get()

                country_regex = re.compile("(?<=flag\d{2} )\w{3}$")
                country_class = list(filter(country_regex.search, row.css("*::attr(class)").getall()))
                if country_class:
                    result['country'] = country_class[0][-3:].upper()

                time = row.css("td.col3::text")
                if time:
                    result['result'] = time.get().strip()

                note = row.css("td.col4::text")
                if note:
                    result['note'] = note.get().strip()

                heat['results'].append(result)
            event['heats'].append(heat)
        yield event
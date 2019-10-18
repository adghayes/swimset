import scrapy
from scrapy.spiders import SitemapSpider
import scrape # content parsing in separate file

class WikiCrawl(scrapy.Spider):
    """Spider for crawling to event summer olypmics results pages on Wikipedia."""
    name = 'wiki_crawl'
    start_urls = [
        "https://en.wikipedia.org/wiki/Swimming_at_the_Summer_Olympics"
    ]

    def __init__(self, *args, **kwargs):
        super(WikiCrawl, self).__init__(*args, **kwargs)
        self.download_delay = 1

    def parse (self, response):
        results_regex = r'^/wiki/Swimming_at_the_\d{4}_Summer_Olympics_%E2%80%93_.*'
        for rel_link in response.xpath('//a/@href').re(results_regex):
            full_link = response.urljoin(rel_link)
            yield scrapy.Request(full_link, callback = self.parse_child)



    def parse_child (self, response):
        results_page_data = scrape.scrape_results_page(response.text)
        results_page_data.setdefault('url',response.url)
        yield results_page_data
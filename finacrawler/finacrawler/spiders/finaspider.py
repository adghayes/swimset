import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor


class FinaSpider(CrawlSpider):
    name = 'finaspider'
    allowed_domains = ['fina.org']
    start_urls = ['http://www.fina.org/discipline/swimming/results']

    rules = [
        # Paginate through competition list
        Rule(LinkExtractor(allow = r'discipline\/swimming\/results\?page=')),

        # Follow link for all competitions 
        Rule(LinkExtractor(allow = r'\/competition-results\/', 
            tags = ['div'], attrs = ['data-href'])),

        # Follow link for all events
        Rule(LinkExtractor(allow = r'\/competition-detailed-results\/', 
            tags = ['tr'], attrs = ['data-href']), callback = 'parse_event')
    ]

    custom_settings = {
        'DOWNLOAD_DELAY':10
    }

    def parse_event(self, response):

        event = {}
        event['url'] = response.url
        event['competition'] = response.css('.event-info-top h2::text').get()
        event['event-title'] = response.css('.event-title::text').get()
        event['year'] = response.css('.year::text').get()
        event['location-name'] = response.css('.location-name::text').get().strip()
        days = response.css('.day::text').getall()
        if len(days) == 2:
            event['start-day'],event['end-day'] = days
        elif len(days) == 1:
            event['start-day'] = days[0]
        months = response.css('.month::text').getall()
        if len(months) == 2:
            event['start-month'],event['end-month'] = months
        elif len(months) == 1:
            event['start-month'] = months[0]
        
        # Works through table picker to get heat metadata, incl. id for result table
        event['phases'] = []
        for owl in response.css('.competition-detailed-results-phase-title-wrapper'):
            phase = {}
            phase['phase-title'] = owl.css('a::text').get()
            phase['phase-id'] = owl.css('a::attr(data)').get()
            phase['phase-date'] = owl.css('.phase-date::text').get()
            table_selector = '.competition-detailed-results.' + phase['phase-id']
            phase['results'] = self.parse_phase_results(response.css(table_selector))
            if not phase['results']:
                self.logger.warning('On page {response.url} failed to retrive results ' +
                'with selector {table_selector}')
            event['phases'].append(phase)

        return event


    def parse_phase_results(self, table):

        results = []
        header = table.xpath('./thead//th[@class != "show-more"]/@class').getall()
        header = [x if x != 'country' else 'ioc-code' for x in header]
        for row in table.css('tbody tr'):
            # If row is a display, normal row, pull data according to headers
            if row.css(".normal-row"):
                cells = [row.css('.' + x + '::text').get() for x in header]
                result = {k: v for k, v in zip(header, cells) if v}
                results.append(result)
            # If row is hidden row, add additional data to most recent result
            elif row.css('.hidden-row'):
                splits = [td.css('.differential-time::text').get() for td in row.css('td.splits:not(:empty)')]
                if splits:
                    results[-1]['splits'] = splits
                sub_header = row.xpath('.//thead//th[@class != "splits"]/@class').getall()
                if sub_header:
                    members = []
                    for sub_row in row.css('tbody tr'):
                        cells = [sub_row.css('.' + x + '::text').get() for x in sub_header]
                        member = {k: v for k, v in zip(sub_header, cells) if v}
                        if member:
                            members.append(member)
                    if members:
                        results[-1]['members'] = members
        return results
        
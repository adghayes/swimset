import scrapy
from scrapy.spiders import CrawlSpider, Rule
from scrapy.linkextractors import LinkExtractor


class MySpider(CrawlSpider):
    name = 'fina_results'
    allowed_domains = ['fina.org']
    start_urls = ['http://www.fina.org/discipline/swimming/results?page=15']

    rules = [
        # Paginate through competition list
        Rule(LinkExtractor(allow = r'discipline\/swimming\/results\?page=')),

        # Follow link for all competitions 
        Rule(LinkExtractor(allow = r'\/competition-results\/', tags = ['div'], attrs = ['data-href'])),

        # Follow link for all events
        Rule(LinkExtractor(allow = r'\/competition-detailed-results\/', tags = ['tr'], attrs = ['data-href']), callback = 'parse_event')
    ]

    custom_settings = {
        'DOWNLOAD_DELAY':1
    }

    def parse_event(self, response):

        event = {}
        event['url'] = response.url
        event['competition'] = response.css('.event-info-top h2::text').get()
        event['event-title'] = response.css('.event-title::text').get()
        event['year'] = response.css('.year::text').get()
        event['location-name'] = response.css('.location-name::text').get().strip()
        days = response.css('.day::text').getall()
        months = response.css('.month::text').getall()
        event['start-day'] = days[0]
        event['end-day'] = days[1]
        event['start-month'] = months[0]
        event['end-month'] = months[1]
        event['phases'] = []

        # Works through table picker to get heat metadata including id to select each result table
        for owl in response.css('.competition-detailed-results-phase-title-wrapper'):
            phase = {}
            phase['phase-title'] = owl.css('a::text').get()
            phase['phase-id'] = owl.css('a::attr(data)').get()
            phase['phase-date'] = owl.css('.phase-date::text').get()
            table_selector = '.competition-detailed-results.' + phase['phase-id']
            phase['results'] = self.parse_phase_results(response.css(table_selector))
            if not phase['results']:
                self.logger.warning('On page {response.url} failed to retrive results with selector {table_selector}')
            event['phases'].append(phase)

        return event


    def parse_phase_results(self, table):

        # Scraping results from each row where row class matches a class in the table header
        results = []
        header = table.css('thead th:not(.show-more)::attr(class)').getall()
        header = [x if x != 'country' else 'ioc-code' for x in header]
        for row in table.css('tbody tr.normal-row'):
            result = {}
            for field in header:
                cell_value = row.css('.' + field + '::text').get()
                if cell_value:
                    result[field] = cell_value
            results.append(result)

        # Some tables include splits or relay members. Here we detect those hidden rows and adds info.
        # The assumption is that every every normal and detail rows are in the same order, and 
        # rows lacking detail rows are at the end.
        if(table.css('thead th.show-more')):
            detail_rows = table.css('tbody tr.hidden-row')
            detail_rows_per_swimmer = 
            if len(detail_rows) <= len(results):
                for idx, detail_row in enumerate(detail_rows):
                    # Check for splits in the detail table. 
                    splits = []
                    for cell in detail_row.css('td.splits:not(:empty)'):
                        splits.append(cell.css('.differential-time::text').get())
                    if splits:
                        results[idx]['splits'] = splits
                    
            else:
                self.logger.warning('On page {response.url} there are more detail rows than normal rows.' )

        return results

    def parse_subresult(self, subresult):
        return True
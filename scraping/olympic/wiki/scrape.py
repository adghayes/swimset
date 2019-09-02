from bs4 import BeautifulSoup
import json
import re
import unicodedata

def scrape_cell(cell, include_links):
    cell_data = unicodedata.normalize("NFKD",''.join(cell.strings)).strip()
    if include_links:
        links = {}
        for a_tag in cell.find_all("a"):
            if a_tag.string != "":
                links[a_tag.string] = a_tag['href']
        if links != {}:
            cell_data = {'text': cell_data, 'links': links}
    return cell_data

def scrape_table (table):
    if table.name != "table":
        raise Exception("scrape_table was passed a non-table tag or other object.")

    table_data = {}

    label_tag = table.find_previous(re.compile("^b|^h\d"))
    label = scrape_cell(label_tag, False)
    table_data["label"] = label

    headers = [scrape_cell(th, False) for th in table.find_all("th")]
    table_data["headers"] = headers

    table_data["rows"] = []
    for row in table.find_all("tr"):
        row_data = {}
        cells = row.find_all("td")
        if len(cells) == len(headers):
            for idx, cell in enumerate(cells):
                row_data[headers[idx]] = scrape_cell(cell, True)
        else:
            row_data = []   # Unlabeled row data as array, as header count does not match cell count
            for cell in cells:
                row_data.append(scrape_cell(cell, True))
        table_data['rows'].append(row_data)

    return table_data

def scrape_wikitables (page):
    wikitables_data = []
    for wikitable in page.find_all(class_ = "wikitable"):
        wikitables_data.append(scrape_table(wikitable))
    return wikitables_data
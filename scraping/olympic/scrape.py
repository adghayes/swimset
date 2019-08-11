from bs4 import BeautifulSoup
import json
import re

def event (content):
    
    soup = BeautifulSoup(content, 'lxml')
    event = {}
    datalayer = re.search('{.*}',soup.body.script.string)
    if datalayer:
        event['datalayer'] = json.loads(datalayer.group())
    event['heats'] = []
    for section in soup.find_all('section', class_ = 'table-box'):

        heat = {}
        heat['name'] = section.h2.string
        heat['results'] = []

        for row in section.tbody.find_all('tr'):
            result = {}

            col1 = row.find('td',class_ = 'col1')
            if col1 and col1.span:
                result['rank'] = col1.span.string.strip()
            
            col2 = row.find('td',class_ = 'col2')
            if col2:
                if col2.a:
                    result['name'] = col2.a['href']
                country = col2.find('div', class_ = 'profile-row')
                if country and country.span:
                    result['country'] = country.span.string

            col3 = row.find('td', class_ = 'col3')
            if col3:
                result['result'] = col3.string.strip()

            col4 = row.find('td', class_ = 'col4')
            if col4:
                result['note'] = col4.string
            
            heat['results'].append(result)
        
        event['heats'].append(heat)

    return event
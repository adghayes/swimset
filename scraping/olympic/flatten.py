import csv
import json
import re

def json_to_list(filename):
    flat_results = [['city','year','sport','distance','stroke','gender',
                    'relay','heat','name','rank','time','note','country']]
    with open(filename) as json_data:
        raw_data = json.load(json_data)
        for event in raw_data:

            url_re = re.compile('https:\/\/www\.olympic\.org\/(?P<city>\w+)-(?P<year>\d+)\/(?P<sport>\w+)' 
                                '\/(?P<legs>\dx|)(?P<distance>\d+)m.*?(.+?(?P<stroke>\w*style|\w*stroke|medley)).*?(?P<gender>women|men)')
            url_match = url_re.match(event['url'])
            if url_match:
                city = url_match.group('city')
                year = url_match.group('year')
                sport = url_match.group('sport')
                distance = url_match.group('distance')
                stroke = url_match.group('stroke')
                gender = url_match.group('gender')
                relay = True if url_match.group('legs') else False

            for heat in event['heats']:
                for result in heat['results']:
                    time = ''
                    if 'result' in result and result['result']:
                        time = time_to_seconds(result['result'])
                    note = result['note'] if 'note' in result else ''
                    country = result['country'] if 'country' in result else ''
                    rank = convert_rank(result['rank']) if 'rank' in result else ''
                    name = result['name'] if 'name' in result else ''

                    flat_result = [city,year,sport,distance,stroke,gender,relay,heat['name'],name,rank,
                                time,note,country]
                    flat_results.append(flat_result)
    return flat_results
        


def convert_rank(rank):
    rank = rank.strip('.')
    if rank == 'G':
        return 1
    elif rank == 'S':
        return 2
    elif rank == 'B':
        return 3
    else:
        return int(rank)

def time_to_seconds(time):
    try:
        parts = re.split(':|h',time)
        seconds = 0.0
        for idx, part in enumerate(parts):
            seconds += float(part)*60**(len(parts)-idx-1)
        return seconds
    except:
        return ''

def list_to_csv(filename, results):
    with open(filename, mode='w') as results_csv:
        results_writer = csv.writer(results_csv, delimiter=',')
        results_writer.writerows(results)

import sys

if len(sys.argv) - 1 == 2:
    source_json = sys.argv[1]
    target_csv = sys.argv[2]
    list_to_csv(target_csv, json_to_list(source_json))
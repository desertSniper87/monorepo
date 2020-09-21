from SPARQLWrapper import SPARQLWrapper, JSON

sparql = SPARQLWrapper("https://query.wikidata.org/sparql")
sparql.setQuery("""
    #Movies released after 2019
    SELECT DISTINCT ?item ?itemLabel WHERE {
      ?item wdt:P31 wd:Q11424.
      ?item wdt:P577 ?pubdate.
      FILTER(?pubdate >= "2019-01-01T00:00:00Z"^^xsd:dateTime)
      SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
    }
""")
sparql.setReturnFormat(JSON)
results = sparql.query().convert()

for result in results["results"]["bindings"]:
    print(result['itemLabel']['value'])

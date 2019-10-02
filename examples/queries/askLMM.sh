#!/bin/sh

if [ $# -gt 0 ]; then
	FILENAME=$1
	QUERY=$( <$FILENAME )
else 
	QUERY=$(<testQuery.sparql)
	echo "# No query provided. Running a test query: testQuery.sparql"
fi

FORMAT="Accept:application/sparql-results+json" &&
#FORMAT="Accept:application/sparql-results+xml" &&
#FORMAT="" &&
curl -X POST -H "$FORMAT" --data-urlencode "query=$QUERY" http://lmm.cropnet.pl/repositories/LMM
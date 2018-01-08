# Description

This dataset is originated from Stichting AdamNet's overview of Amsterdam's street data, available from this [website](https://adamlink.nl/).

The Turtle file available from [this page](https://adamlink.nl/data) has been converted first into CSV files using [JENA arq](https://jena.apache.org/documentation/query/) as a SPARQL engine and the queries contained in the files _pits.rq_ and _relation.rq_.

The process can be executed by running _extractFiles.sh_, after modifying it to match your situation.

Subsequently, the CSV files are tranformed into NDJSON with the R script _ProduceNDJSON.R_

It must be noticed that Stichting AdamNet is still working on the data, so this dataset can change.

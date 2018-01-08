ARQ_DIR="/Users/SB/Software/apache-jena-3.5.0/bin"
DATA_DIR="/Users/SB/Software/code/E_L/histograph/e_l_data/adamlink"


for i in pits relations;
do
    echo -e "\nGenerating ${i}"
    ${ARQ_DIR}/arq --data ${DATA_DIR}/adamlinkstraten.ttl --query ${DATA_DIR}/${i}.rq --results=CSV > ${DATA_DIR}/${i}.csv
done

echo -e "\nDONE!!\n"
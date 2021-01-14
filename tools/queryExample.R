query<-list()
#Data load
query$pid="fao_commodity_dbquery_adv"
query$layer="fao_commodities_dbquery_adv_layer"
query$csw_server="https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw"
query$csw_version="2.0.2"
query$wfs_server="https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows"
query$wfs_version="1.0.0"
query$feature_geom="true"
query$wms_server="https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms"
query$wms_version="1.1.0"
query$stratgy="ogc_viewparams"
query$par="trade_flow:E;aggregation_method:sum"
query$x=-61.34765625
query$y=10.50292968750001
query$srs="EPSG:4326"
query$dsd=""
#Shiny views
query$geoCol="flag"
query$line.title="Line"
query$line.info="Your message here"
query$line.x="year"
query$line.y="value"
query$line.z="commodity"
query$line.caption="line"
query$pie.title="Pie"
query$pie.info="Your message here"
query$pie.x="commodity"
query$pie.y="value"
query$pie.z="year"
query$pie.caption="pie"
query$box.title="Box"
query$box.info="Your message here"
query$box.x="commodity"
query$box.z="year"
query$box.caption="box"
query$data.title="Data"
query$data.info="Your message here"
query$data.caption="table"
query$panel="line+pie+box+data"

query
#FAOCommodity
#?geoCol=flag&line.title=Line&line.info="Your%20message%20here"&line.x=year&line.y=value&line.z=commodity&line.caption=test&pie.title=Pie&pie.info="Your%20message%20here"&pie.x=commodity&pie.y=value&pie.z=year&pie.caption=test&box.title=Box&box.info="Your%20message%20here"&box.x=commodity&box.y=value&box.z=year&box.caption=test&data.title=Data&data.info="Your%20message%20here"&data.caption=table&panel=line+pie+box+data&pid=fao_commodity_dbquery_adv&layer=fao_commodities_dbquery_adv_layer&csw_server=https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw&csw_version=2.0.2&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&feature_geom=false&strategy=ogc_viewparams&par=trade_flow:E;aggregation_method:sum&srs=EPSG:4326
#FAOCapture
#?line.x=year&line.z=species&pie.x=species&pie.z=year&box.x=species&box.z=year&geoCol=flag&withFlag=TRUE&panel=line+pie+box+data&pid=fao_capture_flag_dbquery_advanced_multiyear&layer=fao_capture_dbquery_layer_adv_multiyear&csw_server=https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw&csw_version=2.0.2&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&feature_geom=false&strategy=ogc_viewparams&par="year:2002+2003+2004+2005+2006+2007+2008+2009+2010+2011+2012+2013+2014+2015+2016+2017+2018;aggregation_method:sum&geom=geometry&srs="EPSG:4326"&dsd=%5B%7B"name":"Flagstate","definition":"Flagging%20country%20of%20the%20fishing%20vessels.","primitiveCode":"flag","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Species","definition":"Species%20captured,%20based%20the%20ASFIS%20list%20of%20species%20for%20fishery%20statistics%20purposes.","primitiveCode":"species","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Area","definition":"FAO%20Major%20area%20for%20statistical%20purposes","primitiveCode":"f_area","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Capture","definition":"Quantity%20of%20fish%20in%20number%20or%20biomass%20harvested%20in%20a%20given%20stratum","primitiveCode":"capture","primitiveType":"xsd:decimal","columnType":"variable","minOccurs":1,"maxOccurs":1,"uom":"t","uomLabel":"metric_ton"%7D,%7B"name":"Type%20of%20water","definition":"Marine%20or%20inland%20area","primitiveCode":"f_area_type","primitiveType":"xsd:string","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Year","definition":"Year","primitiveCode":"year","primitiveType":"xsd:int","columnType":"attribute","minOccurs":1,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"geometry","definition":null,"primitiveCode":"geometry","primitiveType":"gml:MultiPolygonPropertyType","columnType":"attribute","minOccurs":0,"maxOccurs":null,"uom":null,"uomLabel":null%7D,%7B"name":"Aggregation%20method","definition":"Method%20of%20aggregation","primitiveCode":"aggregation_method","primitiveType":"xsd:string","columnType":"attribute","minOccurs":1,"maxOccurs":1,"uom":null,"uomLabel":null%7D%5D
#
#?pid=rdb_cdt_42&layer=rdb_cdt_42&csw_server=https://geonetwork-sdi-lab.d4science.org/geonetwork/srv/eng/csw&csw_version=2.0.2&wfs_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/ows&wfs_version=1.0.0&wms_server=https://geoserver-sdi-lab.d4science.org/geoserver/sdilab_fisheriesatlas/wms&wms_version=1.1.0&feature_geom=false&strategy=ogc_viewparams&par=year:2000+2001+2002+2003+2004+2005+2006+2007+2008+2009+2010+2011+2012+2013+2014+2015+2016+2017;aggregation_method:sum&geom=geometry&x=-7385980.315642005&y=874982.5010446282&srs=EPSG:3857&geoCol=country&line.x=year&line.z=species&pie.x=species&pie.z=year&panel=line+pie+data&dsd=%5B%7B%22name%22:%22country%22,%22definition%22:null,%22primitiveCode%22:%22country%22,%22primitiveType%22:%22xsd:string%22,%22columnType%22:%22attribute%22,%22minOccurs%22:0,%22maxOccurs%22:null,%22uom%22:null,%22uomLabel%22:null%7D,%7B%22name%22:%22species%22,%22definition%22:null,%22primitiveCode%22:%22species%22,%22primitiveType%22:%22xsd:string%22,%22columnType%22:%22attribute%22,%22minOccurs%22:0,%22maxOccurs%22:null,%22uom%22:null,%22uomLabel%22:null%7D,%7B%22name%22:%22year%22,%22definition%22:null,%22primitiveCode%22:%22year%22,%22primitiveType%22:%22xsd:int%22,%22columnType%22:%22attribute%22,%22minOccurs%22:0,%22maxOccurs%22:null,%22uom%22:null,%22uomLabel%22:null%7D,%7B%22name%22:%22catches%22,%22definition%22:null,%22primitiveCode%22:%22catches%22,%22primitiveType%22:%22xsd:decimal%22,%22columnType%22:%22variable%22,%22minOccurs%22:0,%22maxOccurs%22:null,%22uom%22:null,%22uomLabel%22:null%7D,%7B%22name%22:%22geometry%22,%22definition%22:null,%22primitiveCode%22:%22geometry%22,%22primitiveType%22:%22gml:MultiPolygonPropertyType%22,%22columnType%22:%22attribute%22,%22minOccurs%22:0,%22maxOccurs%22:null,%22uom%22:null,%22uomLabel%22:null%7D,%7B%22name%22:%22aggregation_method%22,%22definition%22:null,%22primitiveCode%22:%22aggregation_method%22,%22primitiveType%22:%22xsd:string%22,%22columnType%22:%22attribute%22,%22minOccurs%22:1,%22maxOccurs%22:1,%22uom%22:null,%22uomLabel%22:null%7D%5D
select count(*) as 'Total por municipio'
,cast(IIF(MUNICIPIO<10,
CAST(PROVINCIA as nvarchar)+'0'+cast(MUNICIPIO  as nvarchar),
CAST(PROVINCIA as nvarchar)+cast(MUNICIPIO  as nvarchar)) as FLOAT) 'Municipio'
from CENSO_RD_2010_PERSONAS group by cast(IIF(MUNICIPIO<10,
CAST(PROVINCIA as nvarchar)+'0'+cast(MUNICIPIO  as nvarchar),
CAST(PROVINCIA as nvarchar)+cast(MUNICIPIO  as nvarchar)) as FLOAT)



select count(*) as 'Total por region'
,(case when REGION between 1 and 4  then 2 
 when REGION between 5 and 7 then 4
 when REGION between 8 and 9 then 3
 when REGION=10 then 10 else null end ) as 'grupo_region'
from CENSO_RD_2010_PERSONAS group by (case when REGION between 1 and 4  then 2 
 when REGION between 5 and 7 then 4
 when REGION between 8 and 9 then 3
 when REGION=10 then 10 else null end )


select distinct GRUPO_REGION, ORDEN_REGION, DES_PROVINCIA, ID_PROVINCIA from ENCFT_2021_MIEMBROS

select distinct count(*) over (partition by cast(IIF(MUNICIPIO<10,
CAST(PROVINCIA as nvarchar)+'0'+cast(MUNICIPIO  as nvarchar),
CAST(PROVINCIA as nvarchar)+cast(MUNICIPIO  as nvarchar)) as FLOAT) ) as 'total_mun'
,cast(IIF(MUNICIPIO<10,
CAST(PROVINCIA as nvarchar)+'0'+cast(MUNICIPIO  as nvarchar),
CAST(PROVINCIA as nvarchar)+cast(MUNICIPIO  as nvarchar)) as FLOAT) as 'id_municipio'
,(case when REGION between 1 and 4  then 2 
 when REGION between 5 and 7 then 4
 when REGION between 8 and 9 then 3
 when REGION=10 then 10 else null end ) as 'grupo_region'
from CENSO_RD_2010_PERSONAS
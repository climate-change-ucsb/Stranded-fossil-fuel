clear

discount=1.03.^[0:80];
  

data=readtable('ADVANCE.csv','ReadVariableNames',1); 
ref_region=["ASIA","BRA","CHN","EU28","IND","JPN","LAM","MAF","OECD90+EU","REF","RUS","USA","World"]; 
data_ref=data(data.SCENARIO=="Reference",:); 
data_ref=data_ref(ismember(data_ref.REGION,ref_region),:); 
 %===============================Price====================================== 
Oil=data_ref(data_ref.VARIABLE=="Price|Primary Energy|Oil",:); 
Gas=data_ref(data_ref.VARIABLE=="Price|Primary Energy|Gas",:); 
Coal=data_ref(data_ref.VARIABLE=="Price|Primary Energy|Coal",:); 
Oil(isnan(Oil.x2100)==1,:)=[]; 
Gas(isnan(Gas.x2100)==1,:)=[]; 
Coal(isnan(Coal.x2100)==1,:)=[]; 

year=2020:5:2100; %2000-2100, 5 year group 
year_inter=2020:1:2100; 
%Model classification 
model=Oil.MODEL; 
price_Oil=zeros(length(model),81); 
price_Gas=zeros(length(model),81); 
price_Coal=zeros(length(model),81);

for i=1:length(model) 
  %{ 
  global_Oil_tmp=Oil{ismember(Oil.MODEL,model{i}) & Oil.REGION=="World",10:26}; 
  global_Oil_tmp=global_Oil_tmp(isnan(global_Oil_tmp)==0); 
  global_Oil_tmp=interp1(year_tmp,global_Oil_tmp,year_inter); 
  %} 
    Oil_tmp=Oil{i,9:25}; 
    year_tmp=year(isnan(Oil_tmp)==0); 
    Oil_tmp=Oil_tmp(isnan(Oil_tmp)==0); 
    price_Oil_tmp=interp1(year_tmp,Oil_tmp,year_inter); 
    price_Oil(i,:)=price_Oil_tmp./price_Oil_tmp(1); 
  %{ 
  global_Gas_tmp=Gas{ismember(Gas.MODEL,model{i}) & Gas.REGION=="World",10:26}; 
  global_Gas_tmp=global_Gas_tmp(isnan(global_Gas_tmp)==0); 
  global_Gas_tmp=interp1(year_tmp,global_Gas_tmp,year_inter); 
  %} 
    Gas_tmp=Gas{i,9:25}; 
    year_tmp=year(isnan(Gas_tmp)==0); 
    Gas_tmp=Gas_tmp(isnan(Gas_tmp)==0); 
    price_Gas_tmp=interp1(year_tmp,Gas_tmp,year_inter); 
    price_Gas(i,:)=price_Gas_tmp./price_Gas_tmp(1); 
  %{ 
  global_Coal_tmp=Coal{ismember(Coal.MODEL,model{i}) & Coal.REGION=="World",10:26}; 
  global_Coal_tmp=global_Coal_tmp(isnan(global_Coal_tmp)==0); 
  global_Coal_tmp=interp1(year_tmp,global_Coal_tmp,year_inter); 
  %} 
    Coal_tmp=Coal{i,9:25}; 
    year_tmp=year(isnan(Coal_tmp)==0); 
    Coal_tmp=Coal_tmp(isnan(Coal_tmp)==0); 
    price_Coal_tmp=interp1(year_tmp,Coal_tmp,year_inter); 
    price_Coal(i,:)=price_Coal_tmp./price_Coal_tmp(1); 
end
clear *_tmp
  
Region=Oil.REGION; 
Oil_table=[table(model,Region),array2table(price_Oil)]; 
Gas_table=[table(model,Region),array2table(price_Gas)]; 
Coal_table=[table(model,Region),array2table(price_Coal)]; 

name=Oil_table.Properties.VariableNames(3:83); 
median_price_oil=grpstats(Oil_table,'Region','median','DataVars',name); %GJ to bbl 
name=Gas_table.Properties.VariableNames(3:83); 
median_price_gas=grpstats(Gas_table,'Region','median','DataVars',name); %GJ to mmbtu 
name=Coal_table.Properties.VariableNames(3:83); 
median_price_coal=grpstats(Coal_table,'Region','median','DataVars',name);%GJ to tonne 
  

%%
%=========================Cumulative extraction============================ 
%data_ref=data(data.SCENARIO=="AMPERE3-RefPol",:); 
%data_ref=data_ref(ismember(data_ref.REGION,ref_region),:); 
Oil_production=data_ref(data_ref.VARIABLE=="Primary Energy|Oil",:); 
Gas_production=data_ref(data_ref.VARIABLE=="Primary Energy|Gas",:); 
Coal_production=data_ref(data_ref.VARIABLE=="Primary Energy|Coal",:);

Oil_export=data_ref(data_ref.VARIABLE=="Trade|Primary Energy|Oil|Volume",:); 
Gas_export=data_ref(data_ref.VARIABLE=="Trade|Primary Energy|Gas|Volume",:); 
Coal_export=data_ref(data_ref.VARIABLE=="Trade|Primary Energy|Coal|Volume",:);

Oil_production(isnan(Oil_production.x2100)==1,:)=[]; 
Gas_production(isnan(Gas_production.x2100)==1,:)=[]; 
Coal_production(isnan(Coal_production.x2100)==1,:)=[]; 

Oil_production(Oil_production.MODEL=="REMIND V1.7",:)=[]; 
Gas_production(Gas_production.MODEL=="REMIND V1.7",:)=[]; 
Coal_production(Coal_production.MODEL=="REMIND V1.7",:)=[]; 

Oil_export(isnan(Oil_export.x2100)==1,:)=[]; 
Gas_export(isnan(Gas_export.x2100)==1,:)=[]; 
Coal_export(isnan(Coal_export.x2100)==1,:)=[]; 

model=Oil_export.MODEL; 
production_Oil=zeros(length(model),81); 
production_Gas=zeros(length(model),81); 
production_Coal=zeros(length(model),81); 
year2=2010:5:2100; 
  
for i=1:length(model) 
     Oil_tmp=Oil_production{i,7:25}+Oil_export{i,7:25}; 
     year_tmp=year2(isnan(Oil_tmp)==0); 
     Oil_tmp=Oil_tmp(isnan(Oil_tmp)==0); 
     production=interp1(year_tmp,Oil_tmp,year_inter); 
  if production(1)==0 
    production=1; 
  end 
    production_Oil(i,:)=production/production(1); 
    
    Coal_tmp=Coal_production{i,7:25}+Coal_export{i,7:25}; 
    year_tmp=year2(isnan(Coal_tmp)==0); 
    Coal_tmp=Coal_tmp(isnan(Coal_tmp)==0); 
    production=interp1(year_tmp,Coal_tmp,year_inter); 
    
    if production(1)==0 
       production=1; 
    end 
    production_Coal(i,:)=production/production(1); 

    Gas_tmp=Gas_production{i,7:25}+Gas_export{i,7:25}; 
    year_tmp=year2(isnan(Gas_tmp)==0); 
    Gas_tmp=Gas_tmp(isnan(Gas_tmp)==0); 
    production=interp1(year_tmp,Gas_tmp,year_inter); 
  
   if production(1)==0 
     production=1; 
   end 
    production_Gas(i,:)=production/production(1); 
end 
  

Region=Oil_production.REGION; 
Oil_prod_table=[table(model,Region),array2table(production_Oil)]; 
Gas_prod_table=[table(model,Region),array2table(production_Gas)]; 
Coal_prod_table=[table(model,Region),array2table(production_Coal)]; 
name=Oil_prod_table.Properties.VariableNames(3:83); 
median_prod_oil=grpstats(Oil_prod_table,'Region','median','DataVars',name); %GJ to bbl 
name=Gas_prod_table.Properties.VariableNames(3:83); 
median_prod_gas=grpstats(Gas_prod_table,'Region','median','DataVars',name); %GJ to mmbtu 
name=Coal_prod_table.Properties.VariableNames(3:83); 
median_prod_coal=grpstats(Coal_prod_table,'Region','median','DataVars',name);%GJ to tonne 
%%
%======================================== 
region=readtable('Region.xlsx'); 

scale_Oil=readmatrix('scale.xlsx','Sheet',1);
scale_Gas=readmatrix('scale.xlsx','Sheet',2);
scale_Coal=readmatrix('scale.xlsx','Sheet',3);


real_Oil=readtable('reserve and production.xlsx','Sheet','Oil'); 
real_Oil=join(region(:,[1,5]),real_Oil); 

% billion barrel, billion cubic meter, million ton 
real_Gas=readtable('reserve and production.xlsx','Sheet','Gas'); 
real_Gas=join(region(:,[1,5]),real_Gas); 
real_Coal=readtable('reserve and production.xlsx','Sheet','Coal'); 
real_Lignite=readtable('reserve and production.xlsx','Sheet','Lignite'); 
real_Coal.Production=real_Coal.Production+real_Lignite.Production; 
real_Coal.Reserve=real_Coal.Reserve+real_Lignite.Reserve; 
real_Coal=join(region(:,[1,5]),real_Coal); 
region_t=["ASIA","BRA","CHN","EU28","IND","JPN","LAM","MAF","OECD90+EU","REF","RUS","USA"]; 
model_t=["AIM/CGE V.2","IMAGE 3.0","POLES ADVANCE","WITCH2016"]; 
%% 
asset_Oil_ref=zeros(169,81,length(model_t)); 
asset_Gas_ref=zeros(169,81,length(model_t)); 
asset_Coal_ref=zeros(169,81,length(model_t)); 

prod_Oil_ref=zeros(169,81,length(model_t));
prod_Gas_ref=zeros(169,81,length(model_t)); 
prod_Coal_ref=zeros(169,81,length(model_t)); 

price_Oil_ref=zeros(169,81,length(model_t));
price_Gas_ref=zeros(169,81,length(model_t)); 
priece_Coal_ref=zeros(169,81,length(model_t));
% extraction_cost=readtable('extraction_cost.xlsx'); 
  
oil_cost=readmatrix('supply_cost_oil.xlsx'); 
gas_cost=readmatrix('supply_cost_gas.xlsx'); 
coal_cost=readmatrix('supply_cost_coal.xlsx'); 

Oil_real_price=68.3475;%$/barrel
Gas_real_price=5.42;%$/mmbtu
Coal_real_price=107.02;%$/ton
  
  
for i=1:length(model_t) 
   for j=1:length(region_t) 
      
      Oil_price_tmp=Oil_table(Oil_table.Region==region_t(j) & Oil_table.model==model_t(i),:); 
      Oil_prod_tmp=Oil_prod_table(Oil_prod_table.Region==region_t(j)& Oil_prod_table.model==model_t(i),:); 
     
      if isempty(Oil_price_tmp)==1 
        Oil_price_tmp=Oil_table(Oil_table.Region=="World"&Oil_table.model==model_t(i),:); 
      end 
       
      if isempty(Oil_prod_tmp)==1 
         Oil_prod_tmp=Oil_prod_table(Oil_prod_table.Region=="World" & Oil_prod_table.model==model_t(i),:); 
      end 
   
      prod_tmp=real_Oil{real_Oil.Advance_region==region_t(j),4}; 
      prod_tmp=prod_tmp.*Oil_prod_tmp{:,3:83}; 
      prod_tmp_cum=cumsum(prod_tmp,2)-real_Oil{real_Oil.Advance_region==region_t(j),3}; 
      prod_tmp(prod_tmp_cum>0)=0;   

      prod_tmp_cum2=cumsum(prod_tmp,2);

      oil_cost_tmp=oil_cost(:,real_Oil.Advance_region==region_t(j));

      Oil_extraction=zeros(size(prod_tmp,1),81);

      for kk=1:size(prod_tmp,1)
          value_prv=0;
       for k=1:81
          prod_k=transpose(prod_tmp_cum2(:,k));
          delta=cumsum(oil_cost_tmp)-prod_k;
          cum_oil_cost_tmp=cumsum(oil_cost_tmp);
          if prod_k(kk)==0 | prod_tmp(kk,k)==0
             Oil_extraction(kk,k)=0;
             continue
          end

          delta_tmp=min(find(delta(:,kk)>0));
            if isempty(delta_tmp)==1
               if kk>1
                   Oil_extraction(kk,k)=Oil_extraction(kk-1,k);
               else
                   nn=max(find(oil_cost_tmp>0));
                   Oil_extraction(kk,k)=nn*0.25;
               end
            else
               value1=[0.25:0.25:(delta_tmp-1)*0.25]*oil_cost_tmp(1:delta_tmp-1,kk);
              if delta_tmp>1
                 value2=(prod_k(kk)-cum_oil_cost_tmp(delta_tmp-1,kk))*delta_tmp*0.25;
               else
                 value2=(prod_tmp(kk,k)-0)*delta_tmp*0.25;
              end
               value=value1+value2;
               value_delta=value-value_prv;
               value_prv=value;
               Oil_extraction(kk,k)=value_delta/prod_tmp(kk,k);
               %Oil_extraction(kk,k)=delta_tmp*0.25;
             end
        end
      end
  
  %Oil_extraction=extraction_cost{real_Oil.Advance_region==region_t(j),2}; 
  %Oil_real_price=extraction_cost{real_Oil.Advance_region==region_t(j),5}; 
  
  Oil_price_tmp=Oil_price_tmp{:,3:end}.*Oil_real_price-Oil_extraction; 
  Oil_price_tmp(Oil_price_tmp<0)=0;

  %trillion $ 
  asset_Oil_ref(real_Oil.Advance_region==region_t(j),:,i)=Oil_price_tmp.*prod_tmp/1000;
  prod_Oil_ref(real_Oil.Advance_region==region_t(j),:,i)=prod_tmp;
  price_Oil_ref(real_Oil.Advance_region==region_t(j),:,i)=Oil_price_tmp;
  
  Gas_price_tmp=Gas_table(Gas_table.Region==region_t(j) & Gas_table.model==model_t(i),:); 
  Gas_prod_tmp=Gas_prod_table(Gas_prod_table.Region==region_t(j)& Gas_prod_table.model==model_t(i),:); 
  
  if isempty(Gas_price_tmp)==1 
    Gas_price_tmp=Gas_table(Gas_table.Region=="World"&Gas_table.model==model_t(i),:); 
  end 
  if isempty(Gas_prod_tmp)==1 
     Gas_prod_tmp=Gas_prod_table(Gas_prod_table.Region=="World" & Gas_prod_table.model==model_t(i),:); 
  end
  
  prod_tmp=real_Gas{real_Gas.Advance_region==region_t(j),4}/27.29;%billion mmbtu 
  prod_tmp=prod_tmp.*Gas_prod_tmp{:,3:83}; 
  prod_tmp_cum=cumsum(prod_tmp,2)-real_Gas{real_Gas.Advance_region==region_t(j),3}/27.29; 
  prod_tmp(prod_tmp_cum>0)=0; 
  prod_tmp_cum2=cumsum(prod_tmp,2);
 
  gas_cost_tmp=gas_cost(:,real_Gas.Advance_region==region_t(j));
  
  Gas_extraction=zeros(size(prod_tmp,1),81);

  for kk=1:size(prod_tmp,1)
            value_prv=0;
   for k=1:81
      prod_k=transpose(prod_tmp_cum2(:,k));
      delta=cumsum(gas_cost_tmp)-prod_k;
      cum_gas_cost_tmp=cumsum(gas_cost_tmp);
      delta_tmp=min(find(delta(:,kk)>0));
      if prod_k(kk)==0 | prod_tmp(kk,k)==0
         Gas_extraction(kk,k)=0;
         continue
      end

      delta_tmp=min(find(delta(:,kk)>0));
        if isempty(delta_tmp)==1
            try
           Gas_extraction(kk,k)=Gas_extraction(kk-1,k);
            catch
           Gas_extraction(kk,k)=0;
           end
        else
             value1=[0.25:0.25:(delta_tmp-1)*0.25]*gas_cost_tmp(1:delta_tmp-1,kk);
           if delta_tmp>1
             value2=(prod_k(kk)-max(cum_gas_cost_tmp(1:delta_tmp-1,kk)))*delta_tmp*0.25;
           else
             value2=(prod_tmp(kk,k)-0)*delta_tmp*0.25;
           end  
           value=value1+value2;
           value_delta=value-value_prv;
           value_prv=value;
           Gas_extraction(kk,k)=value_delta/prod_tmp(kk,k);
           %Oil_extraction(kk,k)=delta_tmp*0.25;
        end
   end
  end
  %Gas_extraction=extraction_cost{real_Gas.Advance_region==region_t(j),3};
  
  %Gas_real_price=extraction_cost{real_Gas.Advance_region==region_t(j),6}; 
  
  Gas_price_tmp=Gas_price_tmp{:,3:end}.*Gas_real_price-Gas_extraction; 
  Gas_price_tmp(Gas_price_tmp<0)=0;
  
  %trillion $ 
  asset_Gas_ref(real_Gas.Advance_region==region_t(j),:,i)=Gas_price_tmp.*prod_tmp/1000; 
  prod_Gas_ref(real_Gas.Advance_region==region_t(j),:,i)=prod_tmp;
  price_Gas_ref(real_Gas.Advance_region==region_t(j),:,i)=Gas_price_tmp;
  
  Coal_price_tmp=Coal_table(Coal_table.Region==region_t(j) & Coal_table.model==model_t(i),:); 
  Coal_prod_tmp=Coal_prod_table(Coal_prod_table.Region==region_t(j)& Coal_prod_table.model==model_t(i),:); 
  
  if isempty(Coal_price_tmp)==1 
     Coal_price_tmp=Coal_table(Coal_table.Region=="World"&Coal_table.model==model_t(i),:); 
  end 
  
  if isempty(Coal_prod_tmp)==1 
     Coal_prod_tmp=Coal_prod_table(Coal_prod_table.Region=="World" & Coal_prod_table.model==model_t(i),:); 
  end 
  %Coal_extraction=extraction_cost{real_Coal.Advance_region==region_t(j),4}; 
 % Coal_real_price=extraction_cost{real_Coal.Advance_region==region_t(j),7}; 
  prod_tmp=real_Coal{real_Coal.Advance_region==region_t(j),4}; 
  prod_tmp=prod_tmp.*Coal_prod_tmp{:,3:83}; 
  prod_tmp_cum=cumsum(prod_tmp,2)-real_Coal{real_Coal.Advance_region==region_t(j),3}; 
  prod_tmp(prod_tmp_cum>0)=0; 
  prod_tmp_cum2=cumsum(prod_tmp,2);
 
  coal_cost_tmp=coal_cost(:,real_Coal.Advance_region==region_t(j));
  
  Coal_extraction=zeros(size(prod_tmp,1),81);

  for kk=1:size(prod_tmp,1)
            value_prv=0;
   for k=1:81
      prod_k=transpose(prod_tmp_cum2(:,k));
      delta=cumsum(coal_cost_tmp)-prod_k;
      cum_coal_cost_tmp=cumsum(coal_cost_tmp);
      delta_tmp=min(find(delta(:,kk)>0));
      if prod_k(kk)==0 | prod_tmp(kk,k)==0
         Coal_extraction(kk,k)=0;
         continue
      end

      delta_tmp=min(find(delta(:,kk)>0));
        if isempty(delta_tmp)==1
            try
           Coal_extraction(kk,k)=Coal_extraction(kk-1,k);
            catch
           Coal_extraction(kk,k)=0;
           end
        else
             value1=[0.25:0.25:(delta_tmp-1)*0.25]*coal_cost_tmp(1:delta_tmp-1,kk);
           if delta_tmp>1
             value2=(prod_k(kk)-max(cum_coal_cost_tmp(1:delta_tmp-1,kk)))*delta_tmp*0.25;
           else
             value2=(prod_tmp(kk,k)-0)*delta_tmp*0.25;
           end  
           value=value1+value2;
           value_delta=value-value_prv;
           value_prv=value;
           Coal_extraction(kk,k)=value_delta/prod_tmp(kk,k);
           %Oil_extraction(kk,k)=delta_tmp*0.25;
        end
   end
  end
  %Coal_extraction=extraction_cost{real_Coal.Advance_region==region_t(j),3};
  
  %Coal_real_price=extraction_cost{real_Coal.Advance_region==region_t(j),6}; 
  
  Coal_price_tmp=Coal_price_tmp{:,3:end}.*Coal_real_price-Coal_extraction; 
  Coal_price_tmp(Coal_price_tmp<0)=0;
 
  asset_Coal_ref(real_Coal.Advance_region==region_t(j),:,i)=Coal_price_tmp.*prod_tmp/10^6;
  prod_Coal_ref(real_Coal.Advance_region==region_t(j),:,i)=prod_tmp;
  price_Coal_ref(real_Coal.Advance_region==region_t(j),:,i)=Coal_price_tmp;
  end 
end
  
  
  asset_Oil_ref_discount=asset_Oil_ref./discount; 
  asset_Gas_ref_discount=asset_Gas_ref./discount; 
  asset_Coal_ref_discount=asset_Coal_ref./discount; 
  asset_Oil_ref_discount(asset_Oil_ref_discount<0)=0; 
  asset_Gas_ref_discount(asset_Gas_ref_discount<0)=0; 
  asset_Coal_ref_discount(asset_Coal_ref_discount<0)=0; 
  %} 
  %%
%=======================RCP2.6===================== 
ref_region=["ASIA","BRA","CHN","EU28","IND","JPN","LAM","MAF","OECD90+EU","REF","RUS","USA","World"]; 
data_2C=data(data.SCENARIO=="2020_WB2C",:); 
data_2C=data_2C(ismember(data_2C.REGION,ref_region),:); 
 %===============================Price====================================== 
Oil=data_2C(data_2C.VARIABLE=="Price|Primary Energy|Oil",:); 
Gas=data_2C(data_2C.VARIABLE=="Price|Primary Energy|Gas",:); 
Coal=data_2C(data_2C.VARIABLE=="Price|Primary Energy|Coal",:); 
Oil(isnan(Oil.x2100)==1,:)=[]; 
Gas(isnan(Gas.x2100)==1,:)=[]; 
Coal(isnan(Coal.x2100)==1,:)=[]; 

year=2020:5:2100; %2000-2100, 5 year group 
year_inter=2020:1:2100; 
%Model classification 
model=Oil.MODEL; 
price_Oil=zeros(length(model),81); 
price_Gas=zeros(length(model),81); 
price_Coal=zeros(length(model),81);

for i=1:length(model) 
  %{ 
  global_Oil_tmp=Oil{ismember(Oil.MODEL,model{i}) & Oil.REGION=="World",10:26}; 
  global_Oil_tmp=global_Oil_tmp(isnan(global_Oil_tmp)==0); 
  global_Oil_tmp=interp1(year_tmp,global_Oil_tmp,year_inter); 
  %} 
    Oil_tmp=Oil{i,9:25}; 
    year_tmp=year(isnan(Oil_tmp)==0); 
    Oil_tmp=Oil_tmp(isnan(Oil_tmp)==0); 
    price_Oil_tmp=interp1(year_tmp,Oil_tmp,year_inter); 
    price_Oil(i,:)=price_Oil_tmp./price_Oil_tmp(1); 
  %{ 
  global_Gas_tmp=Gas{ismember(Gas.MODEL,model{i}) & Gas.REGION=="World",10:26}; 
  global_Gas_tmp=global_Gas_tmp(isnan(global_Gas_tmp)==0); 
  global_Gas_tmp=interp1(year_tmp,global_Gas_tmp,year_inter); 
  %} 
    Gas_tmp=Gas{i,9:25}; 
    year_tmp=year(isnan(Gas_tmp)==0); 
    Gas_tmp=Gas_tmp(isnan(Gas_tmp)==0); 
    price_Gas_tmp=interp1(year_tmp,Gas_tmp,year_inter); 
    price_Gas(i,:)=price_Gas_tmp./price_Gas_tmp(1); 
  %{ 
  global_Coal_tmp=Coal{ismember(Coal.MODEL,model{i}) & Coal.REGION=="World",10:26}; 
  global_Coal_tmp=global_Coal_tmp(isnan(global_Coal_tmp)==0); 
  global_Coal_tmp=interp1(year_tmp,global_Coal_tmp,year_inter); 
  %} 
    Coal_tmp=Coal{i,9:25}; 
    year_tmp=year(isnan(Coal_tmp)==0); 
    Coal_tmp=Coal_tmp(isnan(Coal_tmp)==0); 
    price_Coal_tmp=interp1(year_tmp,Coal_tmp,year_inter); 
    price_Coal(i,:)=price_Coal_tmp./price_Coal_tmp(1); 
end
clear *_tmp
  
Region=Oil.REGION; 
Oil_table=[table(model,Region),array2table(price_Oil)]; 
Gas_table=[table(model,Region),array2table(price_Gas)]; 
Coal_table=[table(model,Region),array2table(price_Coal)]; 

name=Oil_table.Properties.VariableNames(3:83); 
median_price_oil=grpstats(Oil_table,'Region','median','DataVars',name); %GJ to bbl 
name=Gas_table.Properties.VariableNames(3:83); 
median_price_gas=grpstats(Gas_table,'Region','median','DataVars',name); %GJ to mmbtu 
name=Coal_table.Properties.VariableNames(3:83); 
median_price_coal=grpstats(Coal_table,'Region','median','DataVars',name);%GJ to tonne 
  

%%
%=========================Cumulative extraction============================ 
%data_2C=data(data.SCENARIO=="AMPERE3-2CPol",:); 
%data_2C=data_2C(ismember(data_2C.REGION,2C_region),:); 
Oil_production=data_2C(data_2C.VARIABLE=="Primary Energy|Oil",:); 
Gas_production=data_2C(data_2C.VARIABLE=="Primary Energy|Gas",:); 
Coal_production=data_2C(data_2C.VARIABLE=="Primary Energy|Coal",:);

Oil_export=data_2C(data_2C.VARIABLE=="Trade|Primary Energy|Oil|Volume",:); 
Gas_export=data_2C(data_2C.VARIABLE=="Trade|Primary Energy|Gas|Volume",:); 
Coal_export=data_2C(data_2C.VARIABLE=="Trade|Primary Energy|Coal|Volume",:);

Oil_production(isnan(Oil_production.x2100)==1,:)=[]; 
Gas_production(isnan(Gas_production.x2100)==1,:)=[]; 
Coal_production(isnan(Coal_production.x2100)==1,:)=[]; 

Oil_production(Oil_production.MODEL=="REMIND V1.7",:)=[]; 
Gas_production(Gas_production.MODEL=="REMIND V1.7",:)=[]; 
Coal_production(Coal_production.MODEL=="REMIND V1.7",:)=[]; 

Oil_production(Oil_production.MODEL=="GCAM4.2_ADVANCEWP6",:)=[]; 
Gas_production(Gas_production.MODEL=="GCAM4.2_ADVANCEWP6",:)=[]; 
Coal_production(Coal_production.MODEL=="GCAM4.2_ADVANCEWP6",:)=[]; 

Oil_export(isnan(Oil_export.x2100)==1,:)=[]; 
Gas_export(isnan(Gas_export.x2100)==1,:)=[]; 
Coal_export(isnan(Coal_export.x2100)==1,:)=[]; 

model=Oil_export.MODEL; 
production_Oil=zeros(length(model),81); 
production_Gas=zeros(length(model),81); 
production_Coal=zeros(length(model),81); 
year2=2010:5:2100; 
  
for i=1:length(model) 
     Oil_tmp=Oil_production{i,7:25}+Oil_export{i,7:25}; 
     year_tmp=year2(isnan(Oil_tmp)==0); 
     Oil_tmp=Oil_tmp(isnan(Oil_tmp)==0); 
     production=interp1(year_tmp,Oil_tmp,year_inter); 
  if production(1)==0 
    production=1; 
  end 
    production_Oil(i,:)=production/production(1); 
    
    Coal_tmp=Coal_production{i,7:25}+Coal_export{i,7:25}; 
    year_tmp=year2(isnan(Coal_tmp)==0); 
    Coal_tmp=Coal_tmp(isnan(Coal_tmp)==0); 
    production=interp1(year_tmp,Coal_tmp,year_inter); 
    if production(1)==0 
       production=1; 
    end 
    production_Coal(i,:)=production/production(1); 

    Gas_tmp=Gas_production{i,7:25}+Gas_export{i,7:25}; 
    year_tmp=year2(isnan(Gas_tmp)==0); 
    Gas_tmp=Gas_tmp(isnan(Gas_tmp)==0); 
    production=interp1(year_tmp,Gas_tmp,year_inter); 
  
   if production(1)==0 
     production=1; 
   end 
    production_Gas(i,:)=production/production(1); 
end 
  

Region=Oil_production.REGION; 
Oil_prod_table=[table(model,Region),array2table(production_Oil)]; 
Gas_prod_table=[table(model,Region),array2table(production_Gas)]; 
Coal_prod_table=[table(model,Region),array2table(production_Coal)]; 
name=Oil_prod_table.Properties.VariableNames(3:83); 
median_prod_oil=grpstats(Oil_prod_table,'Region','median','DataVars',name); %GJ to bbl 
name=Gas_prod_table.Properties.VariableNames(3:83); 
median_prod_gas=grpstats(Gas_prod_table,'Region','median','DataVars',name); %GJ to mmbtu 
name=Coal_prod_table.Properties.VariableNames(3:83); 
median_prod_coal=grpstats(Coal_prod_table,'Region','median','DataVars',name);%GJ to tonne 
%%
%======================================== 
region=readtable('Region.xlsx'); 

scale_Oil=readmatrix('scale.xlsx','Sheet',1);
scale_Gas=readmatrix('scale.xlsx','Sheet',2);
scale_Coal=readmatrix('scale.xlsx','Sheet',3);


real_Oil=readtable('reserve and production.xlsx','Sheet','Oil'); 
real_Oil=join(region(:,[1,5]),real_Oil); 

% billion barrel, billion cubic meter, million ton 
real_Gas=readtable('reserve and production.xlsx','Sheet','Gas'); 
real_Gas=join(region(:,[1,5]),real_Gas); 
real_Coal=readtable('reserve and production.xlsx','Sheet','Coal'); 
real_Lignite=readtable('reserve and production.xlsx','Sheet','Lignite'); 
real_Coal.Production=real_Coal.Production+real_Lignite.Production; 
real_Coal.Reserve=real_Coal.Reserve+real_Lignite.Reserve; 
real_Coal=join(region(:,[1,5]),real_Coal); 
region_t=["ASIA","BRA","CHN","EU28","IND","JPN","LAM","MAF","OECD90+EU","REF","RUS","USA"]; 
model_t=["AIM/CGE V.2","IMAGE 3.0","POLES ADVANCE","WITCH2016"]; 

%%
asset_Oil_2C=zeros(169,81,length(model_t)); 
asset_Gas_2C=zeros(169,81,length(model_t)); 
asset_Coal_2C=zeros(169,81,length(model_t)); 

prod_Oil_2C=zeros(169,81,length(model_t));
prod_Gas_2C=zeros(169,81,length(model_t)); 
prod_Coal_2C=zeros(169,81,length(model_t)); 

price_Oil_2C=zeros(169,81,length(model_t));
price_Gas_2C=zeros(169,81,length(model_t)); 
price_Coal_2C=zeros(169,81,length(model_t)); 
% extraction_cost=readtable('extraction_cost.xlsx'); 
  
oil_cost=readmatrix('supply_cost_oil.xlsx'); 
gas_cost=readmatrix('supply_cost_gas.xlsx'); 
coal_cost=readmatrix('supply_cost_coal.xlsx'); 

Oil_real_price=68.3475;%$/barrel
Gas_real_price=5.42;%$/mmbtu
Coal_real_price=107.02;%$/ton
  
  
for i=1:length(model_t) 
   for j=1:length(region_t)     
      Oil_price_tmp=Oil_table(Oil_table.Region==region_t(j) & Oil_table.model==model_t(i),:); 
      Oil_prod_tmp=Oil_prod_table(Oil_prod_table.Region==region_t(j)& Oil_prod_table.model==model_t(i),:); 
     
      if isempty(Oil_price_tmp)==1 
        Oil_price_tmp=Oil_table(Oil_table.Region=="World"&Oil_table.model==model_t(i),:); 
      end 
       
      if isempty(Oil_prod_tmp)==1 
         Oil_prod_tmp=Oil_prod_table(Oil_prod_table.Region=="World" & Oil_prod_table.model==model_t(i),:); 
      end 
   
      prod_tmp=real_Oil{real_Oil.Advance_region==region_t(j),4}; 
      prod_tmp=prod_tmp.*Oil_prod_tmp{:,3:83}; 
      prod_tmp_cum=cumsum(prod_tmp,2)-real_Oil{real_Oil.Advance_region==region_t(j),3}; 
      prod_tmp(prod_tmp_cum>0)=0;   
      prod_tmp_cum2=cumsum(prod_tmp,2);

      oil_cost_tmp=oil_cost(:,real_Oil.Advance_region==region_t(j));

      Oil_extraction=zeros(size(prod_tmp,1),81);

      for kk=1:size(prod_tmp,1)
          value_prv=0;
       for k=1:81
          prod_k=transpose(prod_tmp_cum2(:,k));
          delta=cumsum(oil_cost_tmp)-prod_k;
          cum_oil_cost_tmp=cumsum(oil_cost_tmp);
          if prod_k(kk)==0 | prod_tmp(kk,k)==0
             Oil_extraction(kk,k)=0;
             continue
          end

          delta_tmp=min(find(delta(:,kk)>0));
            if isempty(delta_tmp)==1
               if kk>1
                   Oil_extraction(kk,k)=Oil_extraction(kk-1,k);
               else
                   nn=max(find(oil_cost_tmp>0));
                   Oil_extraction(kk,k)=nn*0.25;
               end
            else
               value1=[0.25:0.25:(delta_tmp-1)*0.25]*oil_cost_tmp(1:delta_tmp-1,kk);
              if delta_tmp>1
                 value2=(prod_k(kk)-cum_oil_cost_tmp(delta_tmp-1,kk))*delta_tmp*0.25;
               else
                 value2=(prod_tmp(kk,k)-0)*delta_tmp*0.25;
              end
               value=value1+value2;
               value_delta=value-value_prv;
               value_prv=value;
               Oil_extraction(kk,k)=value_delta/prod_tmp(kk,k);
               %Oil_extraction(kk,k)=delta_tmp*0.25;
             end
        end
      end
  
  %Oil_extraction=extraction_cost{real_Oil.Advance_region==region_t(j),2}; 
  %Oil_real_price=extraction_cost{real_Oil.Advance_region==region_t(j),5}; 
  
  Oil_price_tmp=Oil_price_tmp{:,3:end}.*Oil_real_price-Oil_extraction; 
  Oil_price_tmp(Oil_price_tmp<0)=0;

  %trillion $ 
  asset_Oil_2C(real_Oil.Advance_region==region_t(j),:,i)=Oil_price_tmp.*prod_tmp/1000;
  prod_Oil_2C(real_Oil.Advance_region==region_t(j),:,i)=prod_tmp;
  price_Oil_2C(real_Oil.Advance_region==region_t(j),:,i)=Oil_price_tmp;
  
  Gas_price_tmp=Gas_table(Gas_table.Region==region_t(j) & Gas_table.model==model_t(i),:); 
  Gas_prod_tmp=Gas_prod_table(Gas_prod_table.Region==region_t(j)& Gas_prod_table.model==model_t(i),:); 
  
  if isempty(Gas_price_tmp)==1 
    Gas_price_tmp=Gas_table(Gas_table.Region=="World"&Gas_table.model==model_t(i),:); 
  end 
  if isempty(Gas_prod_tmp)==1 
     Gas_prod_tmp=Gas_prod_table(Gas_prod_table.Region=="World" & Gas_prod_table.model==model_t(i),:); 
  end
  
  prod_tmp=real_Gas{real_Gas.Advance_region==region_t(j),4}/27.29;%billion mmbtu 
  prod_tmp=prod_tmp.*Gas_prod_tmp{:,3:83}; 
  prod_tmp_cum=cumsum(prod_tmp,2)-real_Gas{real_Gas.Advance_region==region_t(j),3}/27.29; 
  prod_tmp(prod_tmp_cum>0)=0; 
  prod_tmp_cum2=cumsum(prod_tmp,2);
 
  gas_cost_tmp=gas_cost(:,real_Gas.Advance_region==region_t(j));
  
  Gas_extraction=zeros(size(prod_tmp,1),81);

  for kk=1:size(prod_tmp,1)
            value_prv=0;
   for k=1:81
      prod_k=transpose(prod_tmp_cum2(:,k));
      delta=cumsum(gas_cost_tmp)-prod_k;
      cum_gas_cost_tmp=cumsum(gas_cost_tmp);
      delta_tmp=min(find(delta(:,kk)>0));
      if prod_k(kk)==0 | prod_tmp(kk,k)==0
         Gas_extraction(kk,k)=0;
         continue
      end

      delta_tmp=min(find(delta(:,kk)>0));
        if isempty(delta_tmp)==1
            try
           Gas_extraction(kk,k)=Gas_extraction(kk-1,k);
            catch
           Gas_extraction(kk,k)=0;
           end
        else
             value1=[0.25:0.25:(delta_tmp-1)*0.25]*gas_cost_tmp(1:delta_tmp-1,kk);
           if delta_tmp>1
             value2=(prod_k(kk)-max(cum_gas_cost_tmp(1:delta_tmp-1,kk)))*delta_tmp*0.25;
           else
             value2=(prod_tmp(kk,k)-0)*delta_tmp*0.25;
           end  
           value=value1+value2;
           value_delta=value-value_prv;
           value_prv=value;
           Gas_extraction(kk,k)=value_delta/prod_tmp(kk,k);
           %Oil_extraction(kk,k)=delta_tmp*0.25;
        end
   end
  end
  %Gas_extraction=extraction_cost{real_Gas.Advance_region==region_t(j),3};
  
  %Gas_real_price=extraction_cost{real_Gas.Advance_region==region_t(j),6}; 
  
  Gas_price_tmp=Gas_price_tmp{:,3:end}.*Gas_real_price-Gas_extraction; 
  Gas_price_tmp(Gas_price_tmp<0)=0;
  
  %trillion $ 
  asset_Gas_2C(real_Gas.Advance_region==region_t(j),:,i)=Gas_price_tmp.*prod_tmp/1000; 
  prod_Gas_2C(real_Gas.Advance_region==region_t(j),:,i)=prod_tmp;
  price_Gas_2C(real_Gas.Advance_region==region_t(j),:,i)=Gas_price_tmp;
  
  Coal_price_tmp=Coal_table(Coal_table.Region==region_t(j) & Coal_table.model==model_t(i),:); 
  Coal_prod_tmp=Coal_prod_table(Coal_prod_table.Region==region_t(j)& Coal_prod_table.model==model_t(i),:); 
  
  if isempty(Coal_price_tmp)==1 
     Coal_price_tmp=Coal_table(Coal_table.Region=="World"&Coal_table.model==model_t(i),:); 
  end 
  
  if isempty(Coal_prod_tmp)==1 
     Coal_prod_tmp=Coal_prod_table(Coal_prod_table.Region=="World" & Coal_prod_table.model==model_t(i),:); 
  end 
  %Coal_extraction=extraction_cost{real_Coal.Advance_region==region_t(j),4}; 
 % Coal_real_price=extraction_cost{real_Coal.Advance_region==region_t(j),7}; 
  prod_tmp=real_Coal{real_Coal.Advance_region==region_t(j),4}; 
  prod_tmp=prod_tmp.*Coal_prod_tmp{:,3:83}; 
  prod_tmp_cum=cumsum(prod_tmp,2)-real_Coal{real_Coal.Advance_region==region_t(j),3}; 
  prod_tmp(prod_tmp_cum>0)=0; 
  prod_tmp_cum2=cumsum(prod_tmp,2);
 
  coal_cost_tmp=coal_cost(:,real_Coal.Advance_region==region_t(j));
  
  Coal_extraction=zeros(size(prod_tmp,1),81);

  for kk=1:size(prod_tmp,1)
            value_prv=0;
   for k=1:81
      prod_k=transpose(prod_tmp_cum2(:,k));
      delta=cumsum(coal_cost_tmp)-prod_k;
      cum_coal_cost_tmp=cumsum(coal_cost_tmp);
      delta_tmp=min(find(delta(:,kk)>0));
      if prod_k(kk)==0 | prod_tmp(kk,k)==0
         Coal_extraction(kk,k)=0;
         continue
      end

      delta_tmp=min(find(delta(:,kk)>0));
        if isempty(delta_tmp)==1
            try
           Coal_extraction(kk,k)=Coal_extraction(kk-1,k);
            catch
           Coal_extraction(kk,k)=0;
           end
        else
             value1=[0.25:0.25:(delta_tmp-1)*0.25]*coal_cost_tmp(1:delta_tmp-1,kk);
           if delta_tmp>1
             value2=(prod_k(kk)-max(cum_coal_cost_tmp(1:delta_tmp-1,kk)))*delta_tmp*0.25;
           else
             value2=(prod_tmp(kk,k)-0)*delta_tmp*0.25;
           end  
           value=value1+value2;
           value_delta=value-value_prv;
           value_prv=value;
           Coal_extraction(kk,k)=value_delta/prod_tmp(kk,k);
           %Oil_extraction(kk,k)=delta_tmp*0.25;
        end
   end
  end
  
  Coal_price_tmp=Coal_price_tmp{:,3:end}.*Coal_real_price-Coal_extraction; 
  Coal_price_tmp(Coal_price_tmp<0)=0;

  %trillion $ 
  asset_Coal_2C(real_Coal.Advance_region==region_t(j),:,i)=Coal_price_tmp.*prod_tmp/10^6;
  prod_Coal_2C(real_Coal.Advance_region==region_t(j),:,i)=prod_tmp;
  price_Coal_2C(real_Coal.Advance_region==region_t(j),:,i)=Coal_price_tmp;
  end 
end
  
  
asset_Oil_2C_discount=asset_Oil_2C./discount; 
asset_Gas_2C_discount=asset_Gas_2C./discount; 
asset_Coal_2C_discount=asset_Coal_2C./discount; 
asset_Oil_2C_discount(asset_Oil_2C_discount<0)=0; 
asset_Gas_2C_discount(asset_Gas_2C_discount<0)=0; 
asset_Coal_2C_discount(asset_Coal_2C_discount<0)=0; 
%} 
  
 %%
%=================================================== 
strand_Oil=asset_Oil_ref_discount-asset_Oil_2C_discount; 
strand_Gas=asset_Gas_ref_discount-asset_Gas_2C_discount; 
strand_Coal=asset_Coal_ref_discount-asset_Coal_2C_discount; 

%strand_Oil_m=median(sum(strand_Oil,2),3); 
%strand_Gas_m=median(sum(strand_Gas,2),3); 
%strand_Coal_m=median(sum(strand_Coal,2),3);

strand_time=sum(strand_Oil)+sum(strand_Gas)+sum(strand_Coal);
strand_time=transpose(median(strand_time,3).*discount);

strand_time_country=median(strand_Oil+strand_Gas+strand_Coal,3);
strand_time_country=strand_time_country.*discount;

strand_time_max=max(strand_Oil+strand_Gas+strand_Coal,[],3);
strand_time_max=strand_time_max.*discount;

strand_time_min=min(strand_Oil+strand_Gas+strand_Coal,[],3);
strand_time_min=strand_time_min.*discount;
%2020-2030
strand_Oil_2030=reshape(sum(strand_Oil(:,1:11,:),2),[169,4,1]); 
strand_Gas_2030=reshape(sum(strand_Gas(:,1:11,:),2),[169,4,1]); 
strand_Coal_2030=reshape(sum(strand_Coal(:,1:11,:),2),[169,4,1]);

strand_2030=strand_Oil_2030+strand_Gas_2030+strand_Coal_2030;
strand_Oil_model_2030=sum(strand_Oil_2030)
strand_Gas_model_2030=sum(strand_Gas_2030)
strand_Coal_model_2030=sum(strand_Coal_2030)
strand_model_2030=sum(strand_2030);

strand_m_2030=median(strand_2030,2);
sum(strand_m_2030)

%2020-2050
strand_Oil_2050=reshape(sum(strand_Oil(:,1:31,:),2),[169,4,1]); 
strand_Gas_2050=reshape(sum(strand_Gas(:,1:31,:),2),[169,4,1]); 
strand_Coal_2050=reshape(sum(strand_Coal(:,1:31,:),2),[169,4,1]);

strand_2050=strand_Oil_2050+strand_Gas_2050+strand_Coal_2050;
strand_Oil_model_2050=sum(strand_Oil_2050)
strand_Gas_model_2050=sum(strand_Gas_2050)
strand_Coal_model_2050=sum(strand_Coal_2050)
strand_model_2050=sum(strand_2050);

strand_m_2050=median(strand_2050,2);
sum(strand_m_2050)

%2100
strand_Oil_sum=reshape(sum(strand_Oil,2),[169,4,1]); 
strand_Gas_sum=reshape(sum(strand_Gas,2),[169,4,1]); 
strand_Coal_sum=reshape(sum(strand_Coal,2),[169,4,1]); 

strand=strand_Oil_sum+strand_Gas_sum+strand_Coal_sum;
strand_Oil_model=sum(strand_Oil_sum)
strand_Gas_model=sum(strand_Gas_sum)
strand_Coal_mode=sum(strand_Coal_sum)
strand_model=sum(strand);

%strand_Oil_25=prctile(sum(strand_Oil,2),0,3);
%strand_Gas_25=prctile(sum(strand_Gas,2),0,3);
%strand_Coal_25=prctile(sum(strand_Coal,2),0,3);
strand_Oil_25=min(sum(strand_Oil,2),[],3);
strand_Gas_25=min(sum(strand_Gas,2),[],3);
strand_Coal_25=min(sum(strand_Coal,2),[],3);

%strand_Oil_75=prctile(sum(strand_Oil,2),100,3);
%strand_Gas_75=prctile(sum(strand_Gas,2),100,3);
%strand_Coal_75=prctile(sum(strand_Coal,2),100,3);
strand_Oil_75=max(sum(strand_Oil,2),[],3);
strand_Gas_75=max(sum(strand_Gas,2),[],3);
strand_Coal_75=max(sum(strand_Coal,2),[],3);

%strand_m=strand_Oil_m+strand_Gas_m+strand_Coal_m;
%strand_m(strand_m<0)=0;
strand_m=median(strand,2);
sum(strand_m)

strand_25=strand_Oil_25+strand_Gas_25+strand_Coal_25;
%strand_25(strand_25<0)=0;
sum(strand_25)

strand_75=strand_Oil_75+strand_Gas_75+strand_Coal_75;
%strand_75(strand_75<0)=0;
sum(strand_75)

%beyond 2100
reserve_Oil_ref=ceil((real_Oil.Reserve-sum(prod_Oil_ref,2))./(prod_Oil_ref(:,81,:)));
reserve_Gas_ref=ceil((real_Gas.Reserve-sum(prod_Gas_ref,2))./(prod_Gas_ref(:,81,:))); 
reserve_Coal_ref=ceil((real_Coal.Reserve-sum(prod_Coal_ref,2))./(prod_Coal_ref(:,81,:)));  

reserve_Oil_ref(reserve_Oil_ref==Inf | isnan(reserve_Oil_ref)==1)=0;
reserve_Gas_ref(reserve_Gas_ref==Inf | isnan(reserve_Gas_ref)==1)=0;
reserve_Coal_ref(reserve_Coal_ref==Inf | isnan(reserve_Coal_ref)==1)=0;

reserve_Oil_2C=ceil((real_Oil.Reserve-sum(prod_Oil_2C,2))./(prod_Oil_2C(:,81,:)));
reserve_Gas_2C=ceil((real_Gas.Reserve-sum(prod_Gas_2C,2))./(prod_Gas_2C(:,81,:))); 
reserve_Coal_2C=ceil((real_Coal.Reserve-sum(prod_Coal_2C,2))./(prod_Coal_2C(:,81,:)));  

reserve_Oil_2C(reserve_Oil_2C==Inf | isnan(reserve_Oil_2C)==1)=0;
reserve_Gas_2C(reserve_Gas_2C==Inf | isnan(reserve_Gas_2C)==1)=0;
reserve_Coal_2C(reserve_Coal_2C==Inf | isnan(reserve_Coal_2C)==1)=0;
reserve_Coal_2C(reserve_Coal_2C<0)=0;

asset_2100_ref=prod_Oil_ref(:,81,:)/1000.*price_Oil_ref(:,81,:).*((1/1.03)^81*(1-(1/1.03).^reserve_Oil_ref)/(1-1/1.03))+...
               prod_Gas_ref(:,81,:)/1000.*price_Gas_ref(:,81,:).*((1/1.03)^81*(1-(1/1.03).^reserve_Gas_ref)/(1-1/1.03))+...
               prod_Coal_ref(:,81,:)/10^6.*price_Coal_ref(:,81,:).*((1/1.03)^81*(1-(1/1.03).^reserve_Coal_ref)/(1-1/1.03));

asset_2100_2C=prod_Oil_2C(:,81,:)/1000.*price_Oil_2C(:,81,:).*((1/1.03)^81*(1-(1/1.03).^reserve_Oil_2C)/(1-1/1.03))+...
              prod_Gas_2C(:,81,:)/1000.*price_Gas_2C(:,81,:).*((1/1.03)^81*(1-(1/1.03).^reserve_Gas_2C)/(1-1/1.03))+...
              prod_Coal_2C(:,81,:)/10^6.*price_Coal_2C(:,81,:).*((1/1.03)^81*(1-(1/1.03).^reserve_Coal_2C)/(1-1/1.03));
           
 sum(asset_2100_ref)-sum(asset_2100_2C)

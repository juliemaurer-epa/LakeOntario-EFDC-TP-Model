%% =============NETCDF output for Lake Ontario===============
clear
%atmos = '/work/GLFBREEZ/LOEM/MATLAB_Scripts/';
atmos18 = '/work/GLHABS/GreatLakesEco/LakeOntario/Model_Runs/2018/LO_scenario3/NETCDF/';
output_file1 = 'median_TP_zones_LOS3.txt';
output_file2 = 'median_TP_zones_5m_30m_LOS3.txt';

FToRead18 = strcat(atmos18,'gomdom.000000.nc'); %Read NETCDF
FToRead = FToRead18;

GoMDOM = ncinfo(FToRead);

%%================Read GoMDOM Ontario Output==================
LatG = double(ncread(FToRead,'latitude'));
LonG = double(ncread(FToRead,'longitude'));
Lat = double(ncread(FToRead,'ylat'));
Lon = double(ncread(FToRead,'xlon'));
time = ncread(FToRead,'time');
cellthick = ncread(FToRead,'dz');
celldepth = ncread(FToRead,'h');
landmask = double(ncread(FToRead,'fm'));


%Establish model dateref
modstart=datetime(2018,4,1,3,0,0);
modstart2=modstart:1:datetime(2018,9,30,3,0,0);
moddate=datetime(modstart2);
clear modstart2 modstart


% Establish limits in space and time for the NETCDF being read for COMT
tstart=[1 1 1 1]; %Longitude,Latitude,Depth,Time
tcount=[256 133 10 inf]; %Longitude,Latitude,Depth,Time
tstride=[1 1 1 1]; %Longitude,Latitude,Depth,Time

% Establish limits in space and time for the NETCDF being read for COMT
tstarts=[1 1 1]; %Longitude,Latitude,Depth,Time
tcounts=[256 133 10]; %Longitude,Latitude,Depth,Time
tstrides=[1 1 1]; %Longitude,Latitude,Depth,Time


%Read GoMDOM
TR = double(ncread(FToRead,'TR',tstart,tcount,tstride));

for i = 1:size(TR,4)    
    data=TR(:,:,:,i);
    data(landmask==0)=NaN;
    TR1(:,:,:,i)=data;    
end

% Process cell depths.
celldepth(landmask == 0) = NaN;

%%==============Define Zones for Analysis=====================

%=========================================== Zone 1 =======================================================
X1 = [LonG(87,48)]; X2 = [LonG(87,69)]; X3 = [LonG(106,66)]; X4 = [LonG(106,50)]; X5 = [LonG(87,48)];
Y1 = [LatG(87,48)]; Y2 = [LatG(87,69)]; Y3 = [LatG(106,66)]; Y4 = [LatG(106,50)]; Y5 = [LatG(87,48)];

X = [X1,X2,X3,X4,X5];
Y = [Y1,Y2,Y3,Y4,Y5];
[zone1refin,zone1refon] = inpolygon(LonG,LatG,X,Y); % returns 1 if is inside the polygon
zone1ref = zone1refin > 0 | zone1refon > 0;
[z1lonref, z1latref] = find(zone1ref > 0);

%=========================================== Zone 2 =======================================================
X1 = [LonG(146,48)]; X2 = [LonG(148,65)]; X3 = [LonG(153,67)]; X4 = [LonG(153,48)]; X5 = [LonG(146,48)];
Y1 = [LatG(146,48)]; Y2 = [LatG(148,65)]; Y3 = [LatG(153,67)]; Y4 = [LatG(153,48)]; Y5 = [LatG(146,48)];

X = [X1,X2,X3,X4,X5];
Y = [Y1,Y2,Y3,Y4,Y5];
[zone2refin,zone2refon] = inpolygon(LonG,LatG,X,Y); % returns 1 because the point (1,1) is inside the polygon
zone2ref = zone2refin > 0 | zone2refon > 0;
[z2lonref, z2latref] = find(zone2ref > 0);

%=========================================== Zone 3 =======================================================
X1 = [LonG(170,11)]; X2 = [LonG(170,49)]; X3 = [LonG(174,49)]; X4 = [LonG(174,3)]; X5 = [LonG(170,11)];
Y1 = [LatG(170,11)]; Y2 = [LatG(170,49)]; Y3 = [LatG(174,49)]; Y4 = [LatG(174,3)]; Y5 = [LatG(170,11)];

X = [X1,X2,X3,X4,X5];
Y = [Y1,Y2,Y3,Y4,Y5];
[zone3refin,zone3refon] = inpolygon(LonG,LatG,X,Y); % returns 1 because the point (1,1) is inside the polygon
zone3ref = zone3refin > 0 | zone3refon > 0;
[z3lonref, z3latref] = find(zone3ref > 0);

%=========================================== Zone 4 =======================================================
X1 = [LonG(93,75)]; X2 = [LonG(97,84)]; X3 = [LonG(172,83)]; X4 = [LonG(173,75)]; X5 = [LonG(93,75)];
Y1 = [LatG(93,75)]; Y2 = [LatG(97,84)]; Y3 = [LatG(172,83)]; Y4 = [LatG(173,75)]; Y5 = [LatG(93,75)];

X = [X1,X2,X3,X4,X5];
Y = [Y1,Y2,Y3,Y4,Y5];
[zone4refin,zone4refon] = inpolygon(LonG,LatG,X,Y); % returns 1 because the point (1,1) is inside the polygon
zone4ref = zone4refin > 0 | zone4refon > 0;
[z4lonref, z4latref] = find(zone4ref > 0);

%%========================Select data and plot%%timeseries=================
% Zone 1
for i=1:length(z1latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ1(i,t,p) = TR1(z1lonref(i),z1latref(i),t,p);
    end
    end
end

for i = 1:length(z1latref)
    for k = 1:10
	dcol = celldepth(z1lonref(i),z1latref(i),k);
        if isnan(dcol)
	    hZ1(i) = celldepth(z1lonref(i),z1latref(i),k-1);
            break;
	else
	    hZ1(i) = dcol;
	end
    end
end

% Zone 2  
for i=1:length(z2latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ2(i,t,p) = TR1(z2lonref(i),z2latref(i),t,p);
    end
    end
end

for i = 1:length(z2latref)
    for k = 1:10
	dcol = celldepth(z2lonref(i),z2latref(i),k);
        if isnan(dcol)
	    hZ2(i) = celldepth(z2lonref(i),z2latref(i),k-1);
            break;
	else
	    hZ2(i) = dcol;
	end
    end
end

% Zone 3
for i=1:length(z3latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ3(i,t,p) = TR1(z3lonref(i),z3latref(i),t,p);
    end
    end
end

for i = 1:length(z3latref)
    for k = 1:10
	dcol = celldepth(z3lonref(i),z3latref(i),k);
        if isnan(dcol)
	    hZ3(i) = celldepth(z3lonref(i),z3latref(i),k-1);
            break;
	else
	    hZ3(i) = dcol;
	end
    end
end

% Zone 4
for i=1:length(z4latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ4(i,t,p) = TR1(z4lonref(i),z4latref(i),t,p);
    end
    end
end

for i = 1:length(z4latref)
    for k = 1:10
	dcol = celldepth(z4lonref(i),z4latref(i),k);
        if isnan(dcol)
	    hZ4(i) = celldepth(z4lonref(i),z4latref(i),k-1);
            break;
	else
	    hZ4(i) = dcol;
	end
    end
end
	
% Calculate median of TP concentrations at each zone for the summer months (June -- August). 
median_zone1 = median(dataZ1(:,:,62:153), "all", "omitnan");
median_zone2 = median(dataZ2(:,:,62:153), "all", "omitnan");
median_zone3 = median(dataZ3(:,:,62:153), "all", "omitnan");
median_zone4 = median(dataZ4(:,:,62:153), "all", "omitnan");

% Writing a table to a text file
data = array2table([median_zone1, median_zone2, median_zone3, median_zone4]);
data.Properties.VariableNames = {'Eighteenmile_Creek', 'Hamlin_Beach', 'Ontario_Beach', 'Offshore_Zone'}; 
writetable(data, output_file1); % Write the table to a text file

% Zone 1
ia = 0;
ib = 0;
ic = 0;
for i = 1:length(z1latref)
    if hZ1(i) < 5
      ia = ia + 1;
      dataZ1a(ia,:,:) = dataZ1(i,:,:);
    elseif (hZ1(i) >= 5) && (hZ1(i) <= 30)
      ib = ib + 1;
      dataZ1b(ib,:,:) = dataZ1(i,:,:);
    else
      ic = ic + 1;  
      dataZ1c(ic,:,:) = dataZ1(i,:,:);	
    end
end

ia = 0;
ib = 0;
ic = 0;
% Zone 2
for i = 1:length(z2latref)
    if hZ2(i) < 5
       ia = ia + 1;
       dataZ2a(ia,:,:) = dataZ2(i,:,:);
    elseif (hZ2(i) >= 5) && (hZ2(i) <= 30)
       ib = ib + 1;
       dataZ2b(ib,:,:) = dataZ2(i,:,:);
    else
       ic = ic + 1;	 
       dataZ2c(ic,:,:) = dataZ2(i,:,:);	
    end
end	  

ia = 0;
ib = 0;
ic = 0;    
% Zone 3
for i = 1:length(z3latref)
    if hZ3(i) < 5
       ia = ia + 1;
       dataZ3a(ia,:,:) = dataZ3(i,:,:);
    elseif (hZ3(i) >= 5) && (hZ3(i) <= 30)
       ib = ib + 1;
       dataZ3b(ib,:,:) = dataZ3(i,:,:);
    else
       ic = ic + 1;
       dataZ3c(ic,:,:) = dataZ3(i,:,:);	
    end
end	

ia = 0;
ib = 0;
ic = 0;
% Zone 4
for i = 1:length(z4latref)
    if hZ4(i) < 5
       ia = ia + 1;
       dataZ4a(ia,:,:) = dataZ4(i,:,:);
    elseif (hZ4(i) >= 5) && (hZ4(i) <= 30)
        ib = ib + 1;
       dataZ4b(ib,:,:) = dataZ4(i,:,:);
    else
       ic = ic + 1;
       dataZ4c(ic,:,:) = dataZ4(i,:,:);
    end
end
	  
% Calculate median of TP concentrations at each zone for the summer months (June -- August). 
median_zone1a = median(dataZ1a(:,:,62:153), "all", "omitnan");
median_zone1b = median(dataZ1b(:,:,62:153), "all", "omitnan");
median_zone1c = median(dataZ1c(:,:,62:153), "all", "omitnan");

median_zone2a = median(dataZ2a(:,:,62:153), "all", "omitnan");
median_zone2b = median(dataZ2b(:,:,62:153), "all", "omitnan");
median_zone2c = median(dataZ2c(:,:,62:153), "all", "omitnan");

median_zone3a = median(dataZ3a(:,:,62:153), "all", "omitnan");
median_zone3b = median(dataZ3b(:,:,62:153), "all", "omitnan");
if max(hZ3) <= 30
  median_zone3c = NaN;
else
   median_zone3c = median(dataZ3c(:,:,62:153), "all", "omitnan");
end

if min(hZ4) > 30
  median_zone4a = NaN;
  median_zone4b = NaN;
else
  median_zone4a = median(dataZ4a(:,:,62:153), "all", "omitnan");
  median_zone4b = median(dataZ4b(:,:,62:153), "all", "omitnan");
end

median_zone4c = median(dataZ4c(:,:,62:153), "all", "omitnan");

% Writing a table to a text file
data = array2table([median_zone1a, median_zone1b, median_zone1c, median_zone2a, median_zone2b, median_zone2c, ...
		    median_zone3a, median_zone3b, median_zone3c, median_zone4a, median_zone4b, median_zone4c]);
data.Properties.VariableNames = {'Eighteenmile_Creek_5m', 'Eighteenmile_Creek_5m_30m', 'Eighteenmile_Creek_30m', 'Hamlin_Beach_5m', 'Hamlin_Beach_5m_30m', 'Hamlin_Beach_30m', 'Ontario_Beach_5m', 'Ontario_Beach_5m_30m', 'Ontario_Beach_30m', 'Offshore_Zone_5m', 'Offshore_Zone_5m_30m', 'Offshore_Zone_30m'};
writetable(data, output_file2); % Write the table to a text file

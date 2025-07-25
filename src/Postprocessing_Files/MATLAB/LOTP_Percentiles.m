%% =============NETCDF output for Lake Ontario===============
clear all
		  
% atmos = '/work/GLFBREEZ/LOEM/MATLAB_Scripts/';

atmos18 = '/work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_11/NETCDF/';

FToRead18 = strcat(atmos18,'gomdom.000000.nc'); %Read NETCDF
FToRead = FToRead18;

title1 = "Eighteenmile Creek, LOTP, Original";
title2 = "Hamlin Beach, LOTP, Original";
title3 = "Ontario Beach, LOTP, Original";
title4 = "Offshore, LOTP, Original";

outfile1 = "LOTP_Orig_Percentiles_Zone1.png";
outfile2 = "LOTP_Orig_Percentiles_Zone2.png";
outfile3 = "LOTP_Orig_Percentiles_Zone3.png";
outfile4 = "LOTP_Orig_Percentiles_Zone4.png";


GoMDOM = ncinfo(FToRead);

field_data = readtable("TP_observationdata_combined_v3.csv");
fdata = field_data(field_data.date_time >= "04/01/2018" & field_data.date_time <= "09/30/2018",:);

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


%Read GoMDOM
TR = double(ncread(FToRead,'TR',tstart,tcount,tstride));

for i = 1:size(TR,4)    
    data = TR(:,:,:,i);
    data(landmask==0) = NaN;
    TR1(:,:,:,i) = data;    
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

indices = find(ismember(fdata.I_Index,z1lonref) & ismember(fdata.J_Index,z1latref));
fdataZ1 = fdata(indices,:);

% Zone 2  
for i=1:length(z2latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ2(i,t,p) = TR1(z2lonref(i),z2latref(i),t,p);
    end
    end
end

indices = find(ismember(fdata.I_Index,z2lonref) & ismember(fdata.J_Index,z2latref));
fdataZ2 = fdata(indices,:);

% Zone 3
for i=1:length(z3latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ3(i,t,p) = TR1(z3lonref(i),z3latref(i),t,p);
    end
    end
end

indices = find(ismember(fdata.I_Index,z3lonref) & ismember(fdata.J_Index,z3latref));
fdataZ3 = fdata(indices,:);

% Zone 4
for i=1:length(z4latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ4(i,t,p) = TR1(z4lonref(i),z4latref(i),t,p);
    end
    end
end

indices = find(ismember(fdata.I_Index,z4lonref) & ismember(fdata.J_Index,z4latref));
fdataZ4 = fdata(indices,:);
	
% Convert units to ug/L.
time = moddate;
dataZ1 = dataZ1 * 1.0E+06;
dataZ2 = dataZ2 * 1.0E+06;
dataZ3 = dataZ3 * 1.0E+06;
dataZ4 = dataZ4 * 1.0E+06;

%===========================================================================================================================
% Zone 1
%===========================================================================================================================  
percentilesZ1 = prctile(dataZ1, [5 50 95], 1:2);
medianZ1 = percentilesZ1(2,:);

X = [time, fliplr(time)];
Y = [percentilesZ1(1,:), fliplr(percentilesZ1(3,:))];

new_x = fdataZ1.date_time;
new_y = fdataZ1.TP_ugL;

figure; % Create a new figure
hold on; % Allow multiple plots on the same axes
% Plot the shaded percentile band
fill(X, Y, [0.8 0.8 0.8], 'EdgeColor', 'none'); % Fill with a light gray color

% Plot the median or mean line
plot(time, medianZ1, 'Color', [0 0 0], 'LineWidth', 1.2); % median line

hold on

plot(new_x, new_y, 'Marker', 'o', 'MarkerSize', 5, 'MarkerEdgeColor','red', 'MarkerFaceColor','red', 'LineStyle', 'none')
box on;

xlabel('Date'); % Set the X-axis label
ylabel('TP Concentration (ugL-1)'); % Set the Y-axis label
title(title1); % Set the plot title

hold off; % Release the hold on the axes

saveas(gcf, outfile1);

%===========================================================================================================================
% Zone 2
%===========================================================================================================================  
percentilesZ2 = prctile(dataZ2, [5 50 95], 1:2);
medianZ2 = percentilesZ2(2,:);

X = [time, fliplr(time)];
Y = [percentilesZ2(1,:), fliplr(percentilesZ2(3,:))];

new_x = fdataZ2.date_time;
new_y = fdataZ2.TP_ugL;

figure; % Create a new figure
hold on; % Allow multiple plots on the same axes
% Plot the shaded percentile band
fill(X, Y, [0.8 0.8 0.8], 'EdgeColor', 'none'); % Fill with a light gray color
ylim([0, 70]);

% Plot the median or mean line
plot(time, medianZ2, 'Color', [0 0 0], 'LineWidth', 1.2); % Example median line

hold on

plot(new_x, new_y, 'Marker', 'o', 'MarkerSize', 5, 'MarkerEdgeColor','red', 'MarkerFaceColor','red', 'LineStyle', 'none');

box on;

xlabel('Date'); % Set the X-axis label
ylabel('TP Concentration (ugL-1)'); % Set the Y-axis label
title(title2); % Set the plot title

hold off; % Release the hold on the axes

saveas(gcf, outfile2);


%===========================================================================================================================
% Zone 3
%===========================================================================================================================  
percentilesZ3 = prctile(dataZ3, [5 50 95], 1:2);
medianZ3 = percentilesZ3(2,:);

X = [time, fliplr(time)];
Y = [percentilesZ3(1,:), fliplr(percentilesZ3(3,:))];

new_x = fdataZ3.date_time;
new_y = fdataZ3.TP_ugL;

figure; % Create a new figure
hold on; % Allow multiple plots on the same axes
% Plot the shaded percentile band
fill(X, Y, [0.8 0.8 0.8], 'EdgeColor', 'none'); % Fill with a light gray color

% Plot the median or mean line
plot(time, medianZ3, 'Color', [0 0 0], 'LineWidth', 1.2); % Example median line

hold on

plot(new_x, new_y, 'Marker', 'o', 'MarkerSize', 5, 'MarkerEdgeColor','red', 'MarkerFaceColor','red', 'LineStyle', 'none');

box on;

xlabel('Date'); % Set the X-axis label
ylabel('TP Concentration (ugL-1)'); % Set the Y-axis label
title(title3); % Set the plot title

hold off; % Release the hold on the axes

saveas(gcf, outfile3);


%===========================================================================================================================
% Zone 4
%===========================================================================================================================  
percentilesZ4 = prctile(dataZ4, [5 50 95], 1:2);
medianZ4 = percentilesZ4(2,:);

X = [time, fliplr(time)];
Y = [percentilesZ4(1,:), fliplr(percentilesZ4(3,:))];

new_x = fdataZ4.date_time;
new_y = fdataZ4.TP_ugL;

figure; % Create a new figure
hold on; % Allow multiple plots on the same axes
% Plot the shaded percentile band
fill(X, Y, [0.8 0.8 0.8], 'EdgeColor', 'none'); % Fill with a light gray color

% Plot the median or mean line
plot(time, medianZ4, 'Color', [0 0 0], 'LineWidth', 1.2); % Example median line

hold on

plot(new_x, new_y, 'Marker', 'o', 'MarkerSize', 5, 'MarkerEdgeColor','red', 'MarkerFaceColor','red', 'LineStyle', 'none');

xlabel('Date'); % Set the X-axis label
ylabel('TP Concentration (ugL-1)'); % Set the Y-axis label
title(title4); % Set the plot title

box on;

hold off; % Release the hold on the axes

saveas(gcf, outfile4);




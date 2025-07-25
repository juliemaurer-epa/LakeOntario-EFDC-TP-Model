%% =============NETCDF output for Lake Ontario===============
clear all
%atmos = '/work/GLFBREEZ/LOEM/MATLAB_Scripts/';
atmos13 = '/work/GLFBREEZ/Lake_Ontario/Model_Runs/2013/LO_13/NETCDF/';
atmos18 = '/work/GLFBREEZ/Lake_Ontario/Model_Runs/2018/LO_02/NETCDF/';


FToRead13 = strcat(atmos13,'gomdom.000000.nc'); %Read NETCDF
FToRead18 = strcat(atmos18,'gomdom.000000.nc'); %Read NETCDF
FToRead = FToRead18

GoMDOM=ncinfo(FToRead);

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
modstart=datenum('4/1/2013 18:00:00');
modstart2=modstart:1:datenum('5/1/2013 18:00:00');
moddate=datenum(modstart2);
clear modstart2 modstart


% Establish limits in space and time for the NETCDF being read for COMT
tstart=[1 1 1 1]; %Longitude,Latitude,Depth,Time
tcount=[inf inf 10 inf]; %Longitude,Latitude,Depth,Time
tstride=[1 1 1 1]; %Longitude,Latitude,Depth,Time

% Establish limits in space and time for the NETCDF being read for COMT
tstarts=[1 1 1]; %Longitude,Latitude,Depth,Time
tcounts=[inf inf inf]; %Longitude,Latitude,Depth,Time
tstrides=[1 1 1]; %Longitude,Latitude,Depth,Time


%Read GoMDOM
%PPI = double(ncread(FToRead,'PD',tstart,tcount,tstride)) + double(ncread(FToRead,'PG',tstart,tcount,tstride));
%FO2 = double(ncread(FToRead,'DOSOC',tstarts,tcounts,tstrides));
%WRI = double(ncread(FToRead,'BMD',tstart,tcount,tstride)) + double(ncread(FToRead,'BMG',tstart,tcount,tstride));
%O2 = double(ncread(FToRead,'DO2',tstart,tcount,tstride));
%NO3 = double(ncread(FToRead,'NO3',tstart,tcount,tstride));
TR = double(ncread(FToRead,'TR',tstart,tcount,tstride));
%DIA = double(ncread(FToRead,'DIA',tstart,tcount,tstride));
%GRE = double(ncread(FToRead,'GRE',tstart,tcount,tstride));
%NFD = double(ncread(FToRead,'NFD',tstart,tcount,tstride));
%NFG = double(ncread(FToRead,'NFG',tstart,tcount,tstride));
%PFD = double(ncread(FToRead,'PFD',tstart,tcount,tstride));
%PFG = double(ncread(FToRead,'PFG',tstart,tcount,tstride));
%IFD = double(ncread(FToRead,'IFD',tstart,tcount,tstride));
%IFG = double(ncread(FToRead,'IFG',tstart,tcount,tstride));

for i=1:size(TR,4)
    
    data=TR(:,:,:,i);
    data(landmask==0)=NaN;
    TR1(:,:,:,i)=data;
    
end



%%==============Define Zones for Analysis=====================

labelref={'Niagara River' 'Hamlin Beach' 'Ontario Beach'};

%Zone 1
X1=[LonG(73,68)], X2=[LonG(73,71)], X3=[LonG(88,71)],X4=[LonG(88,48)], X5=[LonG(73,68)];
Y1=[LatG(73,68)], Y2=[LatG(73,71)], Y3=[LatG(88,71)],Y4=[LatG(88,48)], Y5=[LatG(73,68)];

X=[X1,X2,X3,X4,X5];
Y=[Y1,Y2,Y3,Y4,Y5];
[zone1refin,zone1refon] = inpolygon(LonG,LatG,X,Y); % returns 1 if is inside the polygon
zone1ref=zone1refin>0 | zone1refon>0;
[z1lonref z1latref]=find(zone1ref>0);

%Zone 2
X1=[LonG(146,48)], X2=[LonG(146,66)], X3=[LonG(154,66)],X4=[LonG(154,48)], X5=[LonG(146,48)];
Y1=[LatG(146,48)], Y2=[LatG(146,66)], Y3=[LatG(154,66)],Y4=[LatG(154,48)], Y5=[LatG(146,48)];

X=[X1,X2,X3,X4,X5];
Y=[Y1,Y2,Y3,Y4,Y5];
[zone2refin,zone2refon] = inpolygon(LonG,LatG,X,Y); % returns 1 because the point (1,1) is inside the polygon
zone2ref=zone2refin>0 | zone2refon>0;
[z2lonref z2latref]=find(zone2ref>0);

%Zone 3
X1=[LonG(170,11)], X2=[LonG(170,17)], X3=[LonG(176,17)],X4=[LonG(176,2)], X5=[LonG(170,11)];
Y1=[LatG(170,11)], Y2=[LatG(170,17)], Y3=[LatG(176,17)],Y4=[LatG(176,2)], Y5=[LatG(170,11)];

X=[X1,X2,X3,X4,X5];
Y=[Y1,Y2,Y3,Y4,Y5];
[zone3refin,zone3refon] = inpolygon(LonG,LatG,X,Y); % returns 1 because the point (1,1) is inside the polygon
zone3ref=zone3refin>0 | zone3refon>0;
[z3lonref z3latref]=find(zone3ref>0);

figure
spy(flipud((landmask(:,:,10)==0)'))
hold on
spy(flipud(zone1ref'),'r.')
spy(flipud(zone2ref'),'c.')
spy(flipud(zone3ref'),'g.')
plot(X,Y,'.k')


%%========================Select data and plot%%timeseries=================
for i=1:length(z1latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ1(i,t,p)=TR1(z1lonref(i),z1latref(i),t,p);
    end
    end
end

for i=1:length(z2latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ2(i,t,p)=TR1(z2lonref(i),z2latref(i),t,p);
    end
    end
end

for i=1:length(z3latref)
    for p=1:size(TR1,4)
    for t=1:10
        dataZ3(i,t,p)=TR1(z3lonref(i),z3latref(i),t,p);
    end
    end
end

%Timeseries by sigma depth
fig1=figure
subplot(3,1,1)
plot(squeeze(nanmean(dataZ1(:,1,:),1)),'-o','markersize',3)
hold on
plot(squeeze(nanmean(dataZ2(:,1,:),1)),'-o','markersize',3)
plot(squeeze(nanmean(dataZ3(:,1,:),1)),'-o','markersize',3)
legend(labelref)
title('Sigma1 Surface Tracer')

subplot(3,1,2)
plot(squeeze(nanmean(dataZ1(:,2,:),1)),'-o','markersize',3)
hold on
plot(squeeze(nanmean(dataZ2(:,2,:),1)),'-o','markersize',3)
plot(squeeze(nanmean(dataZ3(:,2,:),1)),'-o','markersize',3)
title('Sigma2 Tracer')

subplot(3,1,3)
plot(squeeze(nanmean(dataZ1(:,3,:),1)),'-o','markersize',3)
hold on
plot(squeeze(nanmean(dataZ2(:,3,:),1)),'-o','markersize',3)
plot(squeeze(nanmean(dataZ3(:,3,:),1)),'-o','markersize',3)
title('Sigma3Tracer')

%Vertical Profiles
fig2=figure

subplot(1,3,1)
plot(nanmean(dataZ1(:,:,1),1),[1:10],'-o','markersize',3)
hold on
plot(nanmean(dataZ2(:,:,1),1),[1:10],'-o','markersize',3)
plot(nanmean(dataZ3(:,:,1),1),[1:10],'-o','markersize',3)
set(gca,'YDir','reverse')
legend(labelref)
title('Vertical Profile Time1')

subplot(1,3,2)
plot(nanmean(dataZ1(:,:,13),1),[1:10],'-o','markersize',3)
hold on
plot(nanmean(dataZ2(:,:,13),1),[1:10],'-o','markersize',3)
plot(nanmean(dataZ3(:,:,13),1),[1:10],'-o','markersize',3)
set(gca,'YDir','reverse')
%legend(labelref)
title('Vertical Profile Time13')

subplot(1,3,3)
plot(nanmean(dataZ1(:,:,26),1),[1:10],'-o','markersize',3)
hold on
plot(nanmean(dataZ2(:,:,26),1),[1:10],'-o','markersize',3)
plot(nanmean(dataZ3(:,:,26),1),[1:10],'-o','markersize',3)
set(gca,'YDir','reverse')
%legend(labelref)
title('Vertical Profile Time26')



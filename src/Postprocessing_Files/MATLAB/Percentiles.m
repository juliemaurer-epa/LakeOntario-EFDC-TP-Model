time = 1:100;
data = randn(100, 50) + repmat(sin(time/10), 50, 1)';
percentiles = prctile(data', [5 50 95]);
median_value = median(data');

plot(time, percentiles(1,:), 'b--') % 5th percentile
hold on
plot(time, percentiles(2,:), 'k-') % 50th percentile (median)
plot(time, percentiles(3,:), 'r--') % 95th percentile
xlabel('Time')
ylabel('Value')
title('Time Series with Percentiles')
legend('5th Percentile', 'Median', '95th Percentile')
grid on
hold off

X = [time, fliplr(time)];
Y = [percentiles(1,:), fliplr(percentiles(3,:))];
figure; % Create a new figure
hold on; % Allow multiple plots on the same axes

% Plot the shaded percentile band
fill(X, Y, [0.8 0.8 0.8]); % Fill with a light gray color

% Optionally, plot the median or mean line
plot(time, median_value, 'b', 'LineWidth', 2); % Example median line

hold off; % Release the hold on the axes

xlabel('Time'); % Set the X-axis label
ylabel('Value'); % Set the Y-axis label
title('Timeseries with Percentile Shading'); % Set the plot title

mcsv = readtable("TP_observationdata_combined_v3.csv");
m1 = mcsv(mcsv.date_time >= "04/01/2018" & mcsv.date_time <= "09/30/2018",:);

indices = find(ismember(m1.I_Index,z1lonref) & ismember(m1.J_Index,z1latref));

m2 = m1(indices,:);

% To add data points to an existing plot in MATLAB, you can use the hold on command to retain
% the current plot and then add new data points using plotting functions like plot, scatter, etc. Here's how you can do it:

Example 1: Adding Points to a Line Plot
Copy the code
% Initial plot
x = 1:10;
y = x.^2;
plot(x, y, '-o'); % Plot initial data
hold on; % Retain the current plot

% Add new data points
new_x = [3, 7];
new_y = [9, 49];
plot(new_x, new_y, 'r*', 'MarkerSize', 10); % Add red stars for new points

hold off; % Release the plot

Example 2: Adding Points with scatter
Copy the code
% Initial scatter plot
x = rand(1, 10);
y = rand(1, 10);
scatter(x, y, 'b'); % Initial scatter plot
hold on;

% Add new points
new_x = [0.5, 0.8];
new_y = [0.4, 0.9];
scatter(new_x, new_y, 'r', 'filled'); % Add red filled points

hold off;

Example 3: Annotating Added Points
Copy the code
% Initial plot
x = 0:0.1:2*pi;
y = sin(x);
plot(x, y, 'b'); % Plot sine wave
hold on;

% Add a specific point
new_x = pi;
new_y = sin(pi);
plot(new_x, new_y, 'ro', 'MarkerSize', 8); % Add a red circle
text(new_x, new_y, '  (π, 0)', 'FontSize', 10); % Annotate the point

hold off;




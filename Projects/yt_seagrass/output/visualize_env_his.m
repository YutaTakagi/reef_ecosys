clear;
clc;

env_his = readtable("/Users/yuta/Documents/TiTech/Nakamura_Lab/seagrass_model/COAWST/reef_ecosys/Projects/yt_seagrass/output/yt_seagrass_01-env_his.csv");


hold on
% yyaxis left
% plot_DIC = plot(env_his.time, env_his.DIC);
% plot_DO = plot(env_his.time, env_his.DO);

% yyaxis left
% plot_PFD = plot(env_his.time, env_his.PFDsurf);

yyaxis left
% plot_drywt = plot(env_his.time, env_his.sg_Dry_Biomass)
plot_CBm = plot(env_his.time, env_his.sg_C_Biomass);

% legend([plot_DIC, plot_DO, plot_CBm], 'DIC', 'DO', 'CBm');


yyaxis right
% plot_drywt = plot(env_his.time, env_his.sg_Dry_Biomass)
plot_DryBm = plot(env_his.time, env_his.sg_Dry_Biomass);

% legend([plot_DIC, plot_DO, plot_DryBm], 'DIC', 'DO', 'DryBm');

legend([plot_DryBm, plot_CBm], 'DryBm', 'CBm');

hold off




figure
hold on
plot_p_sgrass = plot(env_his.time, env_his.p_sgrass);

legend([plot_p_sgrass], 'p_sgrass');

hold off




figure
hold on
plot_DryBm = plot(env_his.time, env_his.sg_Dry_Biomass);
plot_LfDryBm = plot(env_his.time, env_his.lf_Dry_Biomass);
plot_RtDryBm = plot(env_his.time, env_his.rt_Dry_Biomass);

legend([plot_DryBm, plot_LfDryBm, plot_RtDryBm], 'TotDryBm', 'LfDryBm', 'RtDryBm');

hold off




%%

soil = 99;
root = 1;
i = 0;

while(true) 
    fprintf('%i %f %f\n',i,soil,root)
    pause(0.1)

    % flux = -2*(soil/(5+soil));
    % flux = -2*((soil-root)/(5+(soil-root)));
    % flux = 2*(-(soil/(50+soil))+(root/(150+root)));
    flux = -2*(soil/(5+soil))+(root/(150+root));
    soil = soil+flux;
    root = root-flux;


    i=i+1;
end
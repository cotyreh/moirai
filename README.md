# moirai

Step 0: Premarket Prep

Run 00a-yieldcurve and 00b-historicalmodel either late evening or early morning outside of market hours. They
will start a directory that stores interest rate and historical gamma exposure (GEX) data. Update Daily.

Run 00c-initialprint shortly before the market open. It uses currently-traded futures prices to calculate the
implied open price. It also uses the data captured by 00a-yieldcurve and 00b-historicalmodel to generate the 
initial forecast parameters - mu (drift) and sigma (expected move).

Run 00d-downloadchain periodically just before and during market hours. It saves snapshots of the option chain
that 01a-computeGEX will use to update and save the current GEX.

Step 1: Dynamic Forecasting

Run 01a-computeGEX once each minute from open to close. It calculates and saves the current GEX level. The script
assumes CST time and indexes the dataframe accordingly.

Step 2: Visualization

02a-currentframe and 02b-currentplot can be run at any time to generate a visualization of the current forecast.

Only run 999-animation after the market close if you really care to. It creates an animation of the forecasted 
move vs the realized prices throughout the day. It might be better to break the tasks of 999-animation into separate 
scripts as it could take 20+ minutes on slower machines. 


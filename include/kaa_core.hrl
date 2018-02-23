-define(KAA_ENVIRONMENT(Hash), list_to_atom("kaa_env_" ++ Hash)).

% classes from py
-define(DATAFRAME, 'pandas.core.frame.DataFrame').
-define(AXESPLOT, 'matplotlib.AxesSubplot').
-define(GROUPBY, 'pandas.core.groupby.DataFrameGroupBy').
-define(SEABORNPLOT, 'seaborn.axisgrid.*').
-define(SERIES, 'pandas.core.frame.Series').

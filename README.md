# Link-Prediction-of-Nasdaq-s-top-100
Link Prediction: Inferring the Nasdaq 100 network of correlated social media chatter

Some firms may have closely related patterns in the amount of social media chatter about them over time. In this project, I have built and described the undirected networks that link firms based upon the (partial) correlations in their Twitter activity. In this network, two firms are linked if there is a statistically significant correlation in the daily number of Twitter messages that mention them.

The following steps were followed to determine statistically significant links:

1) Calculated the partial correlation coefficients between each node; this is the correlation between any pair of two nodes that remains after adjusting for their common correlations with every other node in the graph.
2) Computed the Fisher’s transformation to approximate the bivariate distributions and to determine the confidence intervals that are used to obtain p-values.
3) Applied the Benjamini-Hochberg adjustment to control for the false discovery rate; and use a threshold of p < 0.05 to identify statistically significant partial correlations.
4) Used the calculations in the above steps to determine the edges among the nodes based, and finally, to construct the network of firms.

I have analyzed what the resulting network reveal about the relationship between the firms as they are described in Twitter. I have also explained whether the edges appear to represent competitive relationships, cooperative relationships, or some other type of connection. For further insight, I have also considered comparing the results to what I would get with alternative thresholds of statistical significance (i.e. p <0.01), or by trying an alternative method for constructing the edges, such as using the overall correlation rather than partial correlations.

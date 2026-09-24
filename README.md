# Update of the OSPAR FC-1
This is a collection of functions to calculate the occurrence of sensitive fish species indicator by OSPAR based on the OSPAR Quality Status Report 2023. The code is based on the idea of assessing the frequency of species' occurrences of in survey hauls as suggested by Probst et al. (2023).
This code is prepared for the OSPAR Intermediate Assessment 2027.
New features include:

- Besides the binomial integration as described  by Probst et al. (2023), the implementation of a new integration method for combining assessment outcomesfrom multiple surveys based on an average probability of an assessment outcome weighted by the proportion of hauls included.
- The possibility to perform assessments by region (OSPAR regions I-V) or seperately by countries within an OSPAR region.
- A function for conducting assessments accross multiple assessment periods.

# References
Lynam, C. P., Bluemel, J. K., and Probst, W. N. 2022. Recovery of Sensitive Fish Species. In: The 2023 Quality Status Report for the Northeast Atlantic, 19 pp. OSPAR Commission, London.
 
OSPAR. 2023. OSPAR Quality status report 2023.

Probst, W. N., Lynam, C. P., Bluemel, J. K., and Clarke, M. 2023. Assessing change in the occurrence of rare species using the binomial distribution. Ecological Indicators, 156.

# To get started

'''' R
require(ggplot2)
''''

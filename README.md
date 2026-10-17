# Maritime Trade and Conflict: An Empirical Study of Nigerian Regions

**Master’s thesis**  
MSc in Economics and Policy Evaluation

## Overview

How does exposure to international maritime trade relate to local conflict? This thesis examines this question in Nigeria, focusing on how differences in local economic conditions shape the relationship between trade integration and stability.

The analysis combines maritime trade indicators, georeferenced conflict events and satellite data in a panel of **179 administrative districts over 2009–2019**.

## Data

The main sources include:

- **UNCTAD:** container port throughput and the Liner Shipping Connectivity Index (LSCI).
- **ACLED:** georeferenced conflict events, focusing on events related to livelihoods and consumption tensions.
- **IMF PortWatch and Nigerian Ports Authority:** identification of Nigeria’s six main commercial ports—Lagos, Tin Can Island, Port Harcourt, Onne, Warri and Calabar—used to construct the distance-to-port component of the trade exposure index.
- **Satellite nighttime lights:** a proxy for local economic development.
- **Geographical data:** district boundaries and distance to major commercial ports.
- **Environmental data:** measures used to explore heterogeneity in local resource availability.

## Empirical approach

We construct a local trade exposure index combining changes in Nigeria’s container port throughput, maritime connectivity and inverse distance to the nearest major commercial port.

Using district and year fixed effects, with standard errors clustered at the district level, we examine the relationship between trade exposure and conflict. We explore heterogeneity across levels of economic development, coastal regions, port specialisations and environmental conditions.

A falsification exercise uses ports with limited commercial relevance to assess whether the findings reflect commercial exposure rather than geographical proximity alone.

## Main findings

Higher trade exposure is associated with lower conflict incidence in poorer districts, while the relationship is weaker in more developed areas. The negative association is more pronounced in eastern coastal areas and near ports predominantly handling imports and vegetable products.

These findings highlight the role of local economic and spatial inequalities in shaping the relationship between international trade and conflict. The estimates are interpreted as associations rather than definitive causal effects.

## Tools

The analysis was conducted in **R**, including data cleaning, integration of spatial and panel data, construction of the exposure index, econometric estimation and visualisation.

## Author

**Alessandra Amicucci**  
Research interests: international trade, development economics and political economy.

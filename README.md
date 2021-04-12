# The Impact of Temperature on the NFL: Project Overview
- Analyzed impact cold weather had on EPA (expected points added) over the last decade.
- Used NFL play by play data from nflfastR package in R and joined it with temperature data from National Oceanic and Atmospheric Administration.
- Divided teams into cold weather teams and warm weather teams depending on the average temperature of their home city.
- Also divided games into cold and warm games based on game temperature.
- Ran a linear regression to estimate EPA/Play using game temperature, wind speed (control), type of game (cold or warm), and type of team (cold or warm).

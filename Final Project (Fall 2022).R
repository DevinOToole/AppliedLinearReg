### Final Project ###
### Applied Linear Regression ###
### Fall 2022 ###
### Devin O'Toole ###

## Initial data cleaning:
library(readxl)
nfl_data = read_excel('NFL Attendance Data (for coding).xlsx', sheet=1)
colnames(nfl_data) = make.names(colnames(nfl_data))
names(nfl_data)[names(nfl_data) == 'Avg..Home.Attendance'] = 'Avg.Home.Attendance'
names(nfl_data)[names(nfl_data) == 'Avg..Home.Attendance..'] = 'Avg.Home.Attendance.Pct'
names(nfl_data)[names(nfl_data) == 'Stadium.Capacity..1000s.'] = 'Stadium.Capacity.1000s'
names(nfl_data)[names(nfl_data) == 'Avg..Ticket....'] = 'Avg.Ticket.Price'
names(nfl_data)[names(nfl_data) == 'Wins.Prev..Season'] = 'Wins.Prev.Season'
names(nfl_data)[names(nfl_data) == 'Home.Wins.Prev..Season'] = 'Home.Wins.Prev.Season'
names(nfl_data)[names(nfl_data) == 'X..Players.in.Top.50.Jersey.Sales'] = 
  'Num.Players.in.Top.50.Jersey.Sales'

## Visuals introducing key variables:
# Region count
get_region = function(MW, NE, SE, SW){
  if (MW == 1){
    return('MW')
  }
  else if (NE == 1){
    return('NE')
  }
  else if (SE == 1){
    return('SE')
  }
  else if (SW == 1){
    return('SW')
  }
  else {
    return('W')
  }
}

nfl_data_region = nfl_data
nfl_data_region["Region"] = NA

for(x in seq_len(nrow(nfl_data))){
  MW = nfl_data[x, 11]
  NE = nfl_data[x, 12]
  SE = nfl_data[x, 13]
  SW = nfl_data[x, 14]
  
  region = get_region(MW, NE, SE, SW)
  nfl_data_region[x, 15] = region
}

library(ggplot2)
ggplot(nfl_data_region, aes(x=reorder(Region, Region, function(x)-length(x)))) + 
  geom_bar(fill='red') +  labs(x='Region', y="Number of teams", 
                               title='Number of NFL teams by Region')

# Wins distribution
ggplot(nfl_data, aes(x=Wins)) + geom_histogram(color="black", fill="blue", bins=16) + 
  ylab('Number of teams') + ggtitle('Number of NFL teams by 2013 Wins') + 
  scale_x_discrete(name="Wins", limits=factor(seq_len(16)))

## Multiple linear regression model (2013):
# Attendance vs. stadium capacity + wins + home wins + prev wins + 
# prev home wins + avg tic
original_model = lm(Avg.Home.Attendance.Pct ~ Stadium.Capacity.1000s + Wins + 
                      Home.Wins + Wins.Prev.Season + Home.Wins.Prev.Season + 
                      Avg.Ticket.Price + Num.Players.in.Top.50.Jersey.Sales + 
                      NE+ SE + MW + SW, data=nfl_data)
summary.lm(original_model)

## Logit model measuring probability of average home attendance 
## being a sellout (attendance % >= 100%):
nfl_data_w_sellout = nfl_data
nfl_data_w_sellout$Sellout = as.numeric(nfl_data_w_sellout$Avg.Home.Attendance.Pct >= 100)
nfl_data_w_sellout$Sellout

sellout_model <- glm(Sellout ~ Stadium.Capacity.1000s + Wins + 
                      Home.Wins + Wins.Prev.Season + Home.Wins.Prev.Season + 
                      Avg.Ticket.Price + Num.Players.in.Top.50.Jersey.Sales + 
                      NE+ SE + MW + SW, family = binomial(link="logit"), 
                    data=nfl_data_w_sellout)

summary(sellout_model)

pred_sellout_model = predict(sellout_model, type="response")
pred_sellout_model

# Finding optimal threshold for sellout model
class_errors = c()
for(i in seq_along(thresholds)){
  threshold = thresholds[i]
  actual_pred_sellout_model = pred_sellout_model >= threshold
  mod_perf = table(actual_pred_sellout_model, nfl_data_w_sellout$Sellout)
  
  # sum up diagonal values b/w predicted class vs true class, and calculate error
  class_error = 1 - sum(diag(mod_perf)) / sum(mod_perf)
  class_errors[i] = class_error
}

plot(thresholds, class_errors)

thresholds[which(class_errors == min(class_errors))]
class_errors[which(class_errors == min(class_errors))]

# McFadden R squared for sellout model
library(pscl)
pR2(sellout_model)['McFadden']

## Removing attendance of teams that have since relocated:
# Visualizing attendance by team
nfl_data_abbreviations = nfl_data
nfl_data_abbreviations$Team = c("SF", "AZ", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", 
                                "CLE", "DAL", "DET", "DEN", "GB", "HOU", "IND", 
                                "JAX", "KC", "MIA", "MIN", "NE", "NO", "NYG", "NYJ", 
                                "OAK", "PHI", "PIT", "SD", "SEA", "STL", "TB", "TEN", "WAS")

relocated_teams = subset(nfl_data_abbreviations, Team == "OAK" | Team == "SD" | Team == "STL")

ggplot(nfl_data_abbreviations,aes(ordered(Team), Avg.Home.Attendance.Pct, color="steelblue")) + 
  ylab("Avg. Home Attendance %") + xlab("Team") + 
  ggtitle("Avg. Attendance % by Team (2013)", 
          subtitle="Teams which have relocated since 2013 in red") + 
  geom_point(color="steelblue") + geom_point(data=relocated_teams, color="red") 

# Original linear regression model without relocated teams
nfl_data_no_relocated_teams = subset(nfl_data_abbreviations, 
                                     Team != "OAK" & Team != "SD" & Team != "STL")

original_model_no_relocated = lm(Avg.Home.Attendance.Pct ~ Stadium.Capacity.1000s + Wins + 
                      Home.Wins + Wins.Prev.Season + Home.Wins.Prev.Season + 
                      Avg.Ticket.Price + Num.Players.in.Top.50.Jersey.Sales + 
                      NE+ SE + MW + SW, data=nfl_data_no_relocated_teams)

summary.lm(original_model_no_relocated)
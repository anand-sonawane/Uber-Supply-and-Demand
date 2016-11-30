#Reading the whole data into an dataframe.
Uber_request_data<-read.csv("Uber request data.csv");

#looking for NA values if any for Pickup.point
length(which(is.na(Uber_request_data$Pickup.point)));
#Answer is 0, hence no NA values.

#looking for NA values if any for Date
length(which(is.na(Uber_request_data$Date)));
#Answer is 0, hence no NA values.

#looking for NA values if any for Request Time
length(which(is.na(Uber_request_data$Request.time)));
#Answer is 0, hence no NA values.

#looking for NA values if any for Status
length(which(is.na(Uber_request_data$Status)));
#Answer is 0, hence no NA values.
#---------------------------------------------------------------------------------------------------------------------
#1. Make a grouped bar chart depicting the hour-wise trip request made at city and airport respectively. 
#You can aggregate the data for all 5 days on the same axis of 24 hours. 
#Each bar should correspond to an hour and pick-up point (city / airport) should be displayed in two colors.

plot_table1 <- table(Uber_request_data$Pickup.point,substr(Uber_request_data$Request.time,0,2));

plot1<-barplot(plot_table1, main="Hour wise trip request made at city and airport ",
        xlab="Hour of Day",ylab = " No of Requests", col=c("darkblue","red"),
        legend = rownames(plot_table1), beside=TRUE)
#----------------------------------------------------------------------------------------------------------------------
#2.2. In the bar chart (question 1), you’ll be able to see 5 major time blocks based on the frequency of requests
#made at the city and airport. You have to now divide the request-time into 5 time-slots described below.
#Make an additional column “Time_Slot” which takes these 5 categorical values depending on the
#request time:
#Pre_Morning
#Morning_Rush
#Day_Time
#Evening_Rush
#Late_Night


index<-c("00","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15","16","17","18","19","20","21","22","23");
values<-c("Pre_Morning","Pre_Morning","Pre_Morning","Pre_Morning",
          "Morning_Rush","Morning_Rush","Morning_Rush","Morning_Rush","Morning_Rush","Morning_Rush",
          "Day_Time","Day_Time","Day_Time","Day_Time","Day_Time","Day_Time","Day_Time",
          "Evening_Rush","Evening_Rush","Evening_Rush","Evening_Rush","Evening_Rush",
          "Late_Night","Late_Night");

Uber_request_data$Time_slot<-values[match(substr(Uber_request_data$Request.time,0,2),index)];
Uber_request_data$Time_slot=as.factor(Uber_request_data$Time_slot)

plot_table2 <- table(Uber_request_data$Pickup.point,Uber_request_data$Time_slot);

plot2<-barplot(plot_table2, main="Trip request made at city and airport according to the Time_slot",
        xlab="Time slot of the Day",ylab = " No of Requests" ,col=c("darkblue","red"),
        legend = rownames(plot_table2), beside=TRUE, ylim = c(0,2000))

#-------------------------------------------------------------------------------------------------
#3. Make a stacked bar chart where each bar represents a time slot and y axis shows the frequency of requests. Different proportions of bars should
#represent the completed, cancelled and no cars available out of the total customer requests.

Uber_request_data$Time_slot<-factor(Uber_request_data$Time_slot, levels=c("Pre_Morning","Morning_Rush","Day_Time","Evening_Rush","Late_Night"));

plot_table3 <- table(Uber_request_data$Status,Uber_request_data$Time_slot);

plot3<-barplot(plot_table3, main="Trip request made at city and airport according to the Time_slot and their status ",
               xlab="Time Slot of the day",ylab = " No of Requests", col=c("yellow","red","darkblue"),
               legend = rownames(plot_table3));

#-------------------------------------------------------------------------------------------------
#Problem 1

Uber_request_data_Morning_rush<-Uber_request_data[which(Uber_request_data$Time_slot=="Morning_Rush"),];

plot_table4 <- table(Uber_request_data_Morning_rush$Status,Uber_request_data_Morning_rush$Pickup.point);

plot4<-barplot(plot_table4, main="Requests made from the Airport and city during the Morning Rush",
               xlab="Pick up point",ylab = " No of Requests", col=c("yellow","red","darkblue"),
               legend = rownames(plot_table4));

#more cancellationn are at the city pickup

Uber_request_data_Morning_rush_city<-Uber_request_data_Morning_rush[which(Uber_request_data_Morning_rush$Pickup.point=="City"),];

slices<-c(length(which(Uber_request_data_Morning_rush_city$Status=="Cancelled")),
          length(which(Uber_request_data_Morning_rush_city$Status=="No Cars Available")),
          length(which(Uber_request_data_Morning_rush_city$Status=="Trip Completed")));
lbls <- c("Cancelled", "No Cars Available", "Trip Completed");
pct <- round(slices/sum(slices)*100);
lbls <- paste(lbls, pct); # add percents to labels 
lbls <- paste(lbls,"%",sep=""); # ad % to labels 
plot5<-pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Issue percentage of Pick point City");


#Cancellation at the airport pickup
Uber_request_data_Morning_rush_Airport<-Uber_request_data_Morning_rush[which(Uber_request_data_Morning_rush$Pickup.point=="Airport"),];

slices<-c(length(which(Uber_request_data_Morning_rush_Airport$Status=="Cancelled")),
          length(which(Uber_request_data_Morning_rush_Airport$Status=="No Cars Available")),
          length(which(Uber_request_data_Morning_rush_Airport$Status=="Trip Completed")));
lbls <- c("Cancelled", "No Cars Available", "Trip Completed");
pct <- round(slices/sum(slices)*100);
lbls <- paste(lbls, pct); # add percents to labels 
lbls <- paste(lbls,"%",sep=""); # ad % to labels 
plot6<-pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="Pie Chart of Issue percentage of Pick point Airport");
#---------------------------
View(Uber_request_data_Morning_rush_city)
no_of_requests_made_in_city_morning<-nrow(Uber_request_data_Morning_rush_city);
no_of_trip_completed_from_city_to_airport_morning<-nrow(Uber_request_data_Morning_rush_city[which(Uber_request_data_Morning_rush_city$Status=="Trip Completed"),]);
no_of_trips_cancelled_from_city_morning<-nrow(Uber_request_data_Morning_rush_city[which(Uber_request_data_Morning_rush_city$Status=="Cancelled"),]);
no_of_requests_made_in_city_morning
no_of_trip_completed_from_city_to_airport_morning
no_of_trips_cancelled_from_city_morning
#-----------------------------------------------------------


#Problem 2

Uber_request_data_Evening_rush<-Uber_request_data[which(Uber_request_data$Time_slot=="Evening_Rush"),];

plot_table7 <- table(Uber_request_data_Evening_rush$Status,Uber_request_data_Evening_rush$Pickup.point);

plot7<-barplot(plot_table7, main="Requests made from the Airport and city during the Evening Rush",
               xlab="Hour of Day",ylab = " No of Requests", col=c("yellow","red","darkblue"),
               legend = rownames(plot_table7));

#no cars available at airport

Uber_request_data_Evening_rush_city<-Uber_request_data_Evening_rush[which(Uber_request_data_Evening_rush$Pickup.point=="City"),];

slices<-c(length(which(Uber_request_data_Evening_rush_city$Status=="Cancelled")),
          length(which(Uber_request_data_Evening_rush_city$Status=="No Cars Available")),
          length(which(Uber_request_data_Evening_rush_city$Status=="Trip Completed")));
lbls <- c("Cancelled", "No Cars Available", "Trip Completed");
pct <- round(slices/sum(slices)*100);
lbls <- paste(lbls, pct); # add percents to labels 
lbls <- paste(lbls,"%",sep=""); # ad % to labels 
plot8<-pie(slices,labels = lbls, col=rainbow(length(lbls)),
           main="Pie Chart of Issue no cars available PickUp Point City");

Uber_request_data_Evening_rush_Airport<-Uber_request_data_Evening_rush[which(Uber_request_data_Evening_rush$Pickup.point=="Airport"),];

slices<-c(length(which(Uber_request_data_Evening_rush_Airport$Status=="Cancelled")),
          length(which(Uber_request_data_Evening_rush_Airport$Status=="No Cars Available")),
          length(which(Uber_request_data_Evening_rush_Airport$Status=="Trip Completed")));
lbls <- c("Cancelled", "No Cars Available", "Trip Completed");
pct <- round(slices/sum(slices)*100);
lbls <- paste(lbls, pct); # add percents to labels 
lbls <- paste(lbls,"%",sep=""); # ad % to labels 
plot9<-pie(slices,labels = lbls, col=rainbow(length(lbls)),
           main="Pie Chart of Issue no cars avaiblale Pickup point Airpport");


View(Uber_request_data_Evening_rush_city)
no_of_requests_made_at_the_airport_evening<-nrow(Uber_request_data_Evening_rush_Airport);
no_of_trip_completed_from_airport_to_city_evening<-nrow(Uber_request_data_Evening_rush_Airport[which(Uber_request_data_Evening_rush_Airport$Status=="Trip Completed"),]);
no_of_instances_when_no_cars_available<-nrow(Uber_request_data_Evening_rush_Airport[which(Uber_request_data_Evening_rush_Airport$Status=="No Cars Available"),]);
no_of_requests_made_at_the_airport_evening
no_of_trip_completed_from_airport_to_city_evening
no_of_instances_when_no_cars_available

#-------------------------------------------------




a = 9
if(a==10){
  print('zaky chundat')
}else if(a>15){
  print('zakky tidak chundat')
}else {
  print('zakky seribu chundat')
}

a=9

i=2
while (i<5) {
  print('i')
  i = i+1
}

for(i in names){
  print(paste('kunlu',i))
}


luas_persegi <- function(s){
  return(s*s)
}

luas_lingkaran <- function(r){
  pi= 3.14
  luas = pi*r^2
  return(luas)
}

berhitung <- function(n){
  for (i in 1:n) {
    print(i)
  }
}

for(i in i){
  print('kucing')
}

names <- c("adi","budi","cindi","santa")


m = matrix(1:12, nrow = 3)

m(1)


users <- data.frame(
  name <- names,
  gender <- c('male','male','female','male'),
  age <- c(10,15,18,17)
)

dataset <- read.csv('database.csv')


library(dyplyr)
library(dplyr)


filter(Excel_Test_Idrus_Fahreza,station == 'Surabaya Kota' & fail>0)

mutate(Excel_Test_Idrus_Fahreza,total_bawaan = success+fail+CMI)
select(Excel_Test_Idrus_Fahreza)

coba <- transmute(Excel_Test_Idrus_Fahreza,total_bawaan = success+fail+CMI)
coba1<-Excel_Test_Idrus_Fahreza
coba1$total_bawaan<- transmute(Excel_Test_Idrus_Fahreza,total_bawaan = success+fail+CMI)


data_a<- Excel_Test_Idrus_Fahreza %>% 
  filter(success>=10&`courier type`=='Rider')%>%select(c(date,name,city,success))%>%
  head(10)


data_b<- Excel_Test_Idrus_Fahreza%>%group_by(city)%>%
  summarise(avg_success=mean(success))

library(ggplot2)

dataset<- read.csv("C:/Users/ASUS/Downloads/Assessment R & Python_ Dataset_superstore_simple.csv")

ggplot(dataset,aes(x=sales,y=profit)) + geom_point(colour='blue')

ggplot(dataset, aes(x=sales))+geom_histogram(bins = 8)

ggplot(dataset,aes(x=category,y=sales))+
  geom_bar(stat = 'identity', width = 0.5, aes(fill=segment))

sales_per_segment <- dataset%>%group_by(segment)%>%
  summarise(total_sales =sum(sales))

ggplot(sales_per_segment,aes(x="",y=total_sales, fill=segment))+
  geom_bar(stat = 'identity',width=1)+coord_polar("y",start=0)


dataset$order_date <- as.Date(dataset$order_date)

dataset$order_month <- as.Date(cut(dataset$order_date,breaks = 'month'))

ggplot(dataset, aes(x=order_month, y=sales))+
  stat_summary(fun = sum, geom='line')

monthly_sales <- dataset%>%group_by(order_month)%>%summarise(sales=sum(sales))

ggplot(monthly_sales,aes(x=order_month,y=sales))+geom_line()+geom_point()


#selfassesment_R

library(lubridate)
library(dplyr)
library(ggplot2)

--Carilah customer_id yang memiliki sales paling besar
question1<-dataset%>%group_by(customer_id)%>%summarise(total_sales=sum(sales))
question1[order(-question1$total_sales),]%>%head(1)

--Sub-category apa saja yang ada di dalam category 'Office Supplies', dan
masing-masing berapa banyak total profitnya?
dataset%>%group_by(sub_category)%>%summarise(total_profit=sum(profit))

--Berapa banyak order yang menghasilkan profit negatif (rugi)?
filter(dataset,profit<0)%>%select(c(order_id,profit))%>%
  summarise(total_loss_order=n())
  
--Antara 3 customer_id ini, mana yang total sales-nya paling banyak: JE-16165,
KH-16510, AD-10180?
question4<-filter(dataset,customer_id =='JE-16165'| customer_id =='KH-16510'|
         customer_id=='AD-10180')%>%group_by(customer_id)%>%
  summarise(total_sales=sum(sales))
question4[order(-question4$total_sales),]

--Buatlah data frame bernama 'yearly_sales' yang berisi total sales, 
jumlah customers, dan total profit tiap tahun. Tahun berapa profit tertinggi 
diperoleh?
yearly_sales <- dataset%>%group_by(Year = year(order_date))%>%
  summarise(total_sales=sum(sales), 
            total_profit=sum(profit),
            n_distinct(customer_id))
yearly_sales[order(-yearly_sales$total_profit),] 
  
--Buatlah scatterplot antara sales dan profit untuk data di tahun 2014-2015 saja,
bedakan warnanya antara tahun 2014 dan tahun 2015. Beri judul 'Sales vs Profit
2014-2015'!
financial_report <- dataset%>%group_by(Year = year(order_date))%>%
  filter(Year == 2014 | Year == 2015)
ggplot(financial_report,aes(x=sales,y=profit)) + 
  geom_point(aes(col= Year))+ 
    labs(title='Scatterplot Sales vs Profit',
       subtitle = 'Based on 2014-2015 Report')+
  xlim(c(0,7500))+ylim(c(-2500,2500))+
  theme(plot.title = element_text(size = 12, face='bold'))

--Buatlah barchart yang berisi total profit dari 10 customer dengan total sales
tertinggi di tahun 2015!
last_question<- dataset%>%group_by(Year = year(order_date),customer_id)%>%
  filter(Year== 2015)%>%summarise(total_sales=sum(sales),total_profit=sum(profit))
final<- last_question[order(-last_question$total_sales),]%>%head(10)
ggplot(final,aes(x=customer_id, y=total_profit,))+
  geom_bar(stat = 'identity', width = 0.5, fill='blue')+
  labs(title = 'Barchart Total Profit from 10 Customer with the Biggest Sales',
       subtitle = 'Based on 2015 Report')+
  theme(plot.title = element_text(size = 12, face='bold'))



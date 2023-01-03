require(tidyverse)


salaries_data_r <- read_csv("Salary_Dataset_with_Extra_Features.csv")

colnames(salaries_data_r)[3] <- "job_title";
colnames(salaries_data_r)[2] <- "company_name"
colnames(salaries_data_r)[8] <- "job_roles"
colnames(salaries_data_r)[5] <-  "salaries_reported"

##Convert rupies to dollars based on conversion rate on 26/12/2022
salaries_data<- salaries_data_r %>% 
  mutate(salary_d = Salary*0.12)


head(salaries_data)

## Top 10 best paying jobs
## Ordenar as barras. Primeiro agrupa, faz mC)dia, corta, ggplot, cria os eixos e ordena as barras
salaries_data %>%
  group_by(job_title) %>%
  summarize(average_salary = mean(salary_d)) %>%
  arrange(desc(average_salary)) %>% 
  slice(1:10) %>% 
  ggplot()+
  geom_col(mapping = aes(x = average_salary ,y=reorder(job_title, average_salary) ), fill="darkblue") +    
  scale_x_continuous(labels=scales::dollar_format()) +          ## Axis labels
  labs(title= "Top 10  best paying jobs")+ theme(plot.title = element_text(hjust = 0.5)) +    ##Titulo
  ylab("Job Title") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+    ##Axis
  geom_text(aes(x = average_salary, y = reorder(job_title, average_salary), label = format(floor(average_salary),digits=10)),position = position_dodge(width = 0.9),hjust = 1.1, color="white", fontface="bold")

##Top 10 worst paying jobs
salaries_data %>%
  group_by(job_title) %>%
  summarize(average_salary = mean(salary_d)) %>%
  arrange(average_salary) %>%
  slice(1:10) %>% 
  ggplot()+
  geom_col(mapping = aes(x = average_salary ,y=reorder(job_title, average_salary) ), fill="darkblue") +    
  geom_col(mapping = aes(x = average_salary ,y=reorder(job_title, average_salary) ), fill="darkblue") +    
  scale_x_continuous(labels=scales::dollar_format()) +          ## Axis labels
  labs(title= "Top 10  worst paying jobs")+ theme(plot.title = element_text(hjust = 0.5)) +    ##Titulo
  ylab("Job Title") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+    ##Axis
  geom_text(aes(x = average_salary, y = reorder(job_title, average_salary), label = format(floor(average_salary),digits=10)),position = position_dodge(width = 0.9),hjust = 1.1, color="white", fontface="bold")


## Top 10 worst paying jobs excluding internships
salaries_data %>%
  filter(!grepl("Intern", job_title)) %>%
  group_by(job_title) %>%
  summarize(average_salary = mean(salary_d)) %>%
  arrange(average_salary) %>%
  slice(1:10) %>% 
  ggplot()+
  geom_col(mapping = aes(x = average_salary ,y=reorder(job_title, average_salary) ), fill="darkblue") +    
  scale_x_continuous(labels=scales::dollar_format()) +          ## Axis labels
  labs(title= "Top 10  worst paying jobs", subtitle = "Excluding internships")+ theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5)) +    ##Titulo
  ylab("Job Title") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+    ##Axis
  geom_text(aes(x = average_salary, y = reorder(job_title, average_salary), label = format(floor(average_salary),digits=10)),position = position_dodge(width = 0.9),hjust = 1.1, color="white", fontface="bold")
  



## Top 10 best paying companies
salaries_data %>% 
  group_by(company_name) %>% 
  summarize(average_salary=mean(salary_d)) %>% 
  arrange(desc(average_salary)) %>% 
  slice(1:10) %>% 
  ggplot()+
    geom_col(mapping=aes(x= average_salary, y=reorder(company_name, average_salary)),fill="darkblue")+
    scale_x_continuous(labels=scales::dollar_format(),expand = expansion(mult = c(0, .18))) +                                     ## Axis labels
    labs(title= "Top 10 best paying companies")+ theme(plot.title = element_text(hjust = 0.5))+      ## Title
    geom_text(aes(x= average_salary, y=reorder(company_name, average_salary), label = format(floor(average_salary),digits=10)),hjust=0, color="black", fontface="bold")+
    ylab("Company Name") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))

## Top 10 worst paying companies
salaries_data %>% 
  group_by(company_name) %>% 
  summarize(average_salary=mean(salary_d)) %>% 
  filter(company_name != "....") %>%
  arrange(average_salary)%>% 
  slice(1:10) %>% 
  ggplot()+
  geom_col(mapping=aes(x= average_salary, y=reorder(company_name, average_salary)),fill="darkblue")+
  scale_x_continuous(labels=scales::dollar_format()) +                                     ## Axis labels
  labs(title= "Top 10 worst paying companies")+ theme(plot.title = element_text(hjust = 0.5))+      ## Title
  geom_text(aes(x= average_salary, y=reorder(company_name, average_salary), label = format(floor(average_salary),digits=10)),hjust=1.2, color="white", fontface="bold")+
  ylab("Company Name") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))



##Salary per locations
salaries_data %>% 
  group_by(Location) %>% 
  summarize(average_salary=mean(salary_d)) %>% 
  arrange(average_salary)%>% 
  ggplot()+
  geom_col(mapping=aes(x= average_salary, y=reorder(Location, average_salary)),fill="darkblue")+
  scale_x_continuous(labels=scales::dollar_format()) +                                     ## Axis labels
  labs(title= "Average salary per city")+ theme(plot.title = element_text(hjust = 0.5))+      ## Title
  geom_text(aes(x= average_salary, y=reorder(Location, average_salary), label = format(floor(average_salary),digits=10)),hjust=1.2, color="white", fontface="bold")+
  ylab("City") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))




##Paying by roles
salaries_data %>% 
  group_by(job_roles) %>% 
  summarize(average_salary=mean(salary_d)) %>% 
  arrange(average_salary)%>% 
  ggplot()+
  geom_col(mapping=aes(x= average_salary, y=reorder(job_roles, average_salary)),fill="darkblue")+
  scale_x_continuous(labels=scales::dollar_format()) +                                     ## Axis labels
  labs(title= "Average salary per roles")+ theme(plot.title = element_text(hjust = 0.5))+      ## Title
  geom_text(aes(x= average_salary, y=reorder(job_roles, average_salary), label = format(floor(average_salary),digits=10)),hjust=1.2, color="white", fontface="bold")+
  ylab("Role") + xlab("Average Salary")+ theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) + theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))


## Average salary Reports
salaries_data %>%
  summarize(average_reports =mean (salaries_reported))
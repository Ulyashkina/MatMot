library("tidyverse") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
#Подготовка к корреляционному анализу
tbl = read.csv("eddypro.csv", skip = 1, na = c("","NA","-9999", "-9999.0"), comment = c("["))
#Преобразуем таблицу
tbl = tbl[-1,] 
tbl 
#Из всего количества данных выбираем те, которые связанны с осенним периодом
tbl=tbl[tbl$DOY > 244 & tbl$DOY < 335,] 
tbl 
#Внимательнее посмотрим на сами переменные и для этого воспользуеся функцией glimpse(), которая более наглядно представляет каждую отдельную переменную
glimpse(tbl) 
#Select() позволяет быстро получить полезное подмножество, используя операции, основанные на именах переменных
tbl = select(tbl, -(roll)) 
#Добавим новые столбцы, которые являются функциями существующих столбцов
tbl = tbl %>% mutate_if(is.character, factor) 
#Здесь мы использовали функию mutate_if которая делает преобразование над колонкой - factor() при полученни значения TRUE от функции первого параметра - is.character()
#Заменяем ненужные нам символы
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
#Yужно сделать такое последовательно много раз(т.к. нам нужно избавиться не только от !), что приведет к совершенно не читаемому результату. Поэтому воспользуеvся оператором пайппинга
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
#Берем каждую колонку таблицы, передаем ее в функию в виде вектора и выдаем результат в виде вектора длинной равной количеству колонок в таблице
sapply(tbl,is.numeric) 
#Подставляем этот вектор в саму таблицу и получаем таблицу состояющую только из интересующих нас колонок
tbl_numeric = tbl[,sapply(tbl,is.numeric)] 
#Получаем таблицу содержащую все остальные колонки
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
#Переходим к корелляционному анализу
cor_td = cor(tbl_numeric) 
cor_td 
#Необходимо избаиться от все строк, где есть хоть одно значение NA
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
#Преобразуем матрицу в таблицу
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
vars
#Соберем все переменные из вектора с именнами переменных в одну формулу
formula1 = as.formula(paste("co2_flux~" , paste(vars,collapse = "+"), sep=""))
#Создаем модель mod
mod = lm(formula1, data=tbl_numeric)
#Сравниваем различия между большой объединяющей группой и ее отдельными подгруппами по mod
anova(mod)
#Получим общую информацию по модели (mod) и смотрим на коэффициент детерминации (R2), который не должен быть меньше 0,7
summary(mod)
#Создаем новую формулу уже с взаимодействиями
formula2 = co2_flux ~ (DOY + co2_flux + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + water_vapor_density + e + specific_humidity + Tdew + un_co2_flux + w.co2_cov + h2o + h2o.1)^2
#Тоже самое делаем со второй моделью
mod1 = lm(formula2, data=tbl_numeric)
#Смотрим, какие переменные и взаимодействия необходимо убрать для созданя идеальной модели
anova(mod1)
#Следим за коэффициентом детерминации
summary(mod1)
#Упрощаем нашу модель. Создаем новую формулу. Убираем ненужные переменные
formula3 = co2_flux ~ (DOY + co2_flux + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + water_vapor_density + e + specific_humidity + Tdew + un_co2_flux + w.co2_cov + h2o + h2o.1)^2 - h2o_mole_fraction:Tdew - h2o_mole_fraction:un_co2_flux - h2o_mixing_ratio:un_co2_flux - water_vapor_density:h2o - Tdew:h2o - un_co2_flux:h2o - un_co2_flux:h2o.1 - w.co2_cov:h2o
mod2 = lm(formula3, data=tbl_numeric)
#Смотрим, что необходимо убрать на этот раз
anova(mod2)
summary(mod2)
#Продолжаем упрощать нашу модель, удаляя ненужные переменные
formula4 = co2_flux ~ (DOY + co2_flux + h2o_molar_density + h2o_mole_fraction + h2o_mixing_ratio + water_vapor_density + e + specific_humidity + Tdew + un_co2_flux + w.co2_cov + h2o + h2o.1)^2 - h2o_mole_fraction:Tdew - h2o_mole_fraction:un_co2_flux - h2o_mixing_ratio:un_co2_flux - water_vapor_density:h2o - Tdew:h2o - un_co2_flux:h2o - un_co2_flux:h2o.1 - w.co2_cov:h2o - w.co2_cov:h2o.1 - un_co2_flux:w.co2_cov - h2o_mole_fraction:h2o
mod3 = lm(formula4, data=tbl_numeric)
#Смотрим, что ненужынх переменных больше нет
anova(mod3)
#Коэффициент детерминации идеальный
summary(mod3)
#модель идеально описывает полученные данные

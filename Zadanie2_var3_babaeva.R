#Бабаева Ксения
#ПАЭ 123 вариант 3
#создайте модель множественной линейной регрессии ночных потоков углекислого газа 
#за летний период 2013 года по данным измерений методом турбулентной пульсации
#задаем директорию 
setwd("E:/Zadanie2/MathModel")
#проверяем директорию
getwd()
#подгружаем пакеты (install) и библиотеки
library("tidyverse") 
library("stringr")    
library("dplyr")      
library("ggplot2")
#Считываем данные из файла eddypro и заменяем  все нечисловые значения на NA и игнорируем строчки с "["
eddypro = read.csv("eddypro.csv", skip = 1, na=c("","NA","-9999","-9999.0"), comment=c("["))
#пропускаем перрвую строчку (она пустая)
eddypro = eddypro[-1, ]
#удаляем ненужный  столбец (он тоже пустой)
eddypro = select(eddypro, -(roll))
# Преобразуем в факторы (factor) столбцы типа char(символ)
eddypro = eddypro %>% mutate_if(is.character, factor)
#Заменяем конфликтующие  символы в названии колонок на допустимые для переменных 
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]", "_exclam_") %>% 
  str_replace_all("[?]", "_quest_") %>% 
  str_replace_all("[*]", "_star_") %>% 
  str_replace_all("[+]", "_plus_") %>%
  str_replace_all("[-]", "_minus_") %>%
  str_replace_all("[@]", "_at_") %>%
  str_replace_all("[$]", "_dollar_") %>%
  str_replace_all("[#]", "_hash_") %>%
  str_replace_all("[/]", "_slash_") %>%
  str_replace_all("[%]", "__pecent_") %>%
  str_replace_all("[&]", "_amp_") %>%
  str_replace_all("[\\^]", "_power_") %>%
  str_replace_all("[()]", "_")

#Превращаем столбцы таблицы в виде векторов для проверки
glimpse(eddypro)
#Убираем NA
eddypro = drop_na(eddypro)
#Отфильтруем данные потоков CO2 за летний период
eddypro = filter(eddypro, DOY >= 152 & DOY < 244)
#Отфильтруем данные потоков CO2 за ночной период
eddypro = filter(eddypro, daytime==FALSE)
#Получим таблицу состоящую только из цифр
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric)]
#Получим таблицу с остальными колонками
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric)]

#Создание непересекающихся выборок
row_numbers = 1:length(eddypro_numeric$co2_flux)
teach = sample(row_numbers, floor(length(eddypro_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#Обучающая выборка
teaching_tbl = eddypro_numeric[teach,]
#Тестирующая выборка
testing_tbl = eddypro_numeric[test,]


#Создадим модели линейной регрессии
#МОДЕЛЬ 1 по обучающей выборке
mod1 = lm(co2_flux~ (.) , data = teaching_tbl)

#Информация о модели (p-значения)
summary(mod1)
#Коэффициенты модели
coef(mod1)
#остатки модели
resid(mod1)
#доверительный интервал
confint(mod1)
#Дисперсионный анализ модели
anova(mod1)
#Графическое представление модели1
plot(mod1)


#МОДЕЛЬ 2 (строим ее исходя из данных таблицы функции anova mod1 с уровнем значимости до 0,01)
mod2 = lm ( co2_flux~  DOY + file_records  + qc_Tau + rand_err_Tau + H  + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux 
            + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg + co2_v.adv + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_mole_fraction + h2o_mixing_ratio  + air_pressure 
            + air_density + air_heat_capacity +  air_molar_volume + water_vapor_density + e + es + specific_humidity + RH
            + Tdew + v_unrot + w_unrot + u_rot + w_rot + max_speed + wind_dir + yaw + pitch + u. + TKE + L + X.z.d..L + bowen_ratio + T.
            + x_offset + x_50. + x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf 
            + un_LE  + LE_scf + un_co2_flux + un_h2o_flux  , data = teaching_tbl)

#Информация о модели (p-значения)
summary(mod2)
#Коэффициенты модели
coef(mod2)
#остатки модели
resid(mod2)
#доверительный интервал
confint(mod2)
#Дисперсионный анализ модели
anova(mod2)
#сравнение моделей
anova(mod1,mod2)
#Графическое представление  модели 2
plot(mod2)


#МОДЕЛЬ 3 (повторим отсеивание аналогичным путем, с помощью функции anova mod2)

mod3 = lm ( co2_flux~ DOY + file_records  + qc_Tau + rand_err_Tau + H  + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux 
            + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg +co2_v.adv + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
            + h2o_mole_fraction + h2o_mixing_ratio  + air_pressure + air_density + air_heat_capacity
            + air_molar_volume + es + specific_humidity + RH + Tdew
            + w_unrot + u_rot + w_rot + yaw + pitch + u. + TKE  + X.z.d..L + bowen_ratio + T. + x_offset
            + x_70. + x_90. + un_Tau + Tau_scf + un_H + H_scf 
            + + un_LE + LE_scf + un_co2_flux + un_h2o_flux ,data = teaching_tbl)

#Информация о модели (p-значения)
summary(mod3)
#Коэффициенты модели
coef(mod3)
#остатки модели
resid(mod3)
#доверительный интервал
confint(mod3)
#Дисперсионный анализ модели
anova(mod3)
#сравнение моделей
anova(mod2,mod3)
#Графическое представление  модели 3
plot(mod3)


#МОДЕЛЬ 4 (аналогично с предыдущей моделью продолжаем отсеивание)

mod4=lm( co2_flux~ DOY + file_records  + qc_Tau + rand_err_Tau + H  + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux 
         + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg +co2_v.adv + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
         + h2o_mole_fraction + h2o_mixing_ratio  + air_pressure + air_density + air_heat_capacity
         + air_molar_volume + es + specific_humidity + RH + Tdew
         + w_unrot + u_rot + w_rot + yaw + u. + TKE  + X.z.d..L + bowen_ratio + T. + x_offset
         + x_70. + x_90. + un_Tau + Tau_scf + un_H  
         + LE_scf + un_co2_flux + un_h2o_flux ,data = teaching_tbl)

#Информация о модели (p-значения)
summary(mod4)
#Коэффициенты модели
coef(mod4)
#остатки модели
resid(mod4)
#доверительный интервал
confint(mod4)
#Дисперсионный анализ модели
anova(mod4)
#сравнение моделей
anova(mod3,mod4)
#Графическое представление  модели 4
plot(mod4)


#МОДЕЛЬ 5 (продолжаем отсеивание)

mod5=lm( co2_flux~ DOY + file_records  + qc_Tau + rand_err_Tau + H  + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux 
         + rand_err_co2_flux + h2o_flux + rand_err_h2o_flux + H_strg +co2_v.adv + h2o_v.adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio
         + h2o_mole_fraction + h2o_mixing_ratio  + air_pressure + air_density + air_heat_capacity
         + air_molar_volume + es + specific_humidity + RH + Tdew
         + w_unrot + u_rot + w_rot + yaw + u. + TKE  + X.z.d..L + bowen_ratio + T. + x_offset
         + x_70. + x_90. + un_Tau + Tau_scf + un_H  
         + LE_scf + un_co2_flux  ,data = teaching_tbl)

#Информация о модели (p-значения)
summary(mod5)
#Коэффициенты модели
coef(mod5)
#остатки модели
resid(mod5)
#доверительный интервал
confint(mod5)
#Дисперсионный анализ модели
anova(mod5)
#сравнение моделей
anova(mod4,mod5)
#Графическое представление  модели 5
plot(mod5)


#Корреляционный анализ переменных участвующих в линейной модели
cor_teaching_tbl = select(teaching_tbl,co2_flux, DOY , file_records  , qc_Tau , rand_err_Tau , H  , rand_err_H , LE , qc_LE , rand_err_LE , qc_co2_flux ,
                          rand_err_co2_flux , h2o_flux , rand_err_h2o_flux , H_strg , co2_v.adv , h2o_v.adv , co2_molar_density , co2_mole_fraction , co2_mixing_ratio ,
                          h2o_mole_fraction , h2o_mixing_ratio  , air_pressure , air_density , air_heat_capacity ,
                          air_molar_volume , es , specific_humidity , RH , Tdew ,
                          w_unrot , u_rot , w_rot , yaw , u. , TKE  , X.z.d..L , bowen_ratio , T. , x_offset ,
                          x_70. , x_90. , un_Tau , Tau_scf , un_H , 
                          LE_scf , un_co2_flux)
#Получаем таблицу коэффициентов корреляций. И подправляем модель 5, убирая из модели одну из двух коррелирующих между собой переменных (начиная от коэффициента >= 0.7)
cor_td = cor(cor_teaching_tbl) %>% as.data.frame

#Теперь получаем графическое представление по модели

#Построим точки co2_flux от co2_flux на значениях ОБУЧАЮЩЕЙ подвыборки 
#Наложим предсказанные значения по модели 5 на ОБУЧАЮЩЕЙ выборке сверху в виде линии
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod5, teaching_tbl)))
#Проведем аналогичные действия на ТЕСТИРУЮЩЕЙ подвыборке
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
#Так как у нас модель зависит от множества переменных, 
#мы можем вывести много графиков зависимостей co2_flux от учитываемых в модели параметров
#Примеры
#Зависимость потоков углекислого газа от дня в году
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
#Зависимость потоков углекислого газа от плотности воздуха
qplot(air_density, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
#Зависимость потоков углекислого газа от потоков воды 
qplot(h2o_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))
#Зависимость потоков углекислого газа от давления воздуха
qplot(air_pressure, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod5, testing_tbl)))

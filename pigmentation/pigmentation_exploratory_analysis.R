
# PIGMENTATION EXPLORATORY ANALYSIS
# Created 8/9/2022

library(tidyverse)
library(gridExtra)



#########

# 1. Combine data from 2 observers and master sheet

raw_pigmentation1 <- read.csv("./data/Caitlynn_PigmentationData.csv")
raw_pigmentation2 <- read.csv("./data/Andy_PigmentationData.csv")
pigmentation_master <- read.csv("./data/Samples_Master.csv")

# Recompute sums
pigmentation1 <- raw_pigmentation1[1:5]
pigmentation2 <- raw_pigmentation2[1:5]

pigmentation1 <- mutate(pigmentation1, "Sum" = T5 + T6 + T7)
pigmentation2 <- mutate(pigmentation2, "Sum" = T5 + T6 + T7)

# Combine observed data
pigmentation1 <- rename(pigmentation1, Rand_ID = Tube.)
pigmentation <- rbind(pigmentation1, pigmentation2)

# Merge using Rand_ID
full_pigmentation <- merge(pigmentation, pigmentation_master)
write.csv(full_pigmentation, "full_pigmentation.csv")

# Notes:
# - pigmentation1 row 1557 malformed
# - no data for Rand_ID 13


##########

# 2. Remove time points 2 and 8 from data

full_pigmentation_removed <- full_pigmentation_data[!(full_pigmentation$TP==2 |full_pigmentation$TP==8),]
write.csv(full_pigmentation_removed, "full_pigmentation_removed.csv")


##########

# 3. Create file with 1 row per sample

pigmentation_samples <- full_pigmentation_removed %>% 
  group_by(Rand_ID) %>%
  summarise(
    T5 = mean(T5),
    T6 = mean(T6),
    T7 = mean(T7),
    Sum = mean(Sum)
  )

# Add back master info lost in previous step
full_pigmentation_samples <- merge(pigmentation_samples, pigmentation_master)

# Organize data by TP (time point) and remove extra columns
full_pigmentation_samples <- arrange(full_pigmentation_samples, TP, Treatment)
full_pigmentation_samples <- select(full_pigmentation_samples, TP, Treatment, Experimenter, Rand_ID, T5, T6, T7, Sum) 

write.csv(full_pigmentation_samples, "full_pigmentation_samples.csv")
summary(full_pigmentation_samples[4:7])


##########

# 4. Create file with 1 row per TP / Treatment

full_pigmentation_removed <- read.csv("full_pigmentation_removed.csv")

# TEMP CODE START, Removed NAs to visualize
full_pigmentation_removed_nadrop <- drop_na(full_pigmentation_removed) 
full_pigmentation_TP_Treatment <- full_pigmentation_removed_nadrop %>% 
  group_by(TP, Treatment) %>%
  summarise(
    se5 = sd(T5)/sqrt(10),
    se6 = sd(T6)/sqrt(10),
    se7 = sd(T7)/sqrt(10),
    seSum = sd(Sum)/sqrt(10),
    T5 = mean(T5),
    T6 = mean(T6),
    T7 = mean(T7),
    Sum = mean(Sum),
    )

write.csv(full_pigmentation_TP_Treatment, "full_pigmentation_TP_Treatment_nadrop2.csv")


# For peace of mind
test <- full_pigmentation_removed_nadrop %>% 
  filter(
    TP == 13,
    Treatment == "P"
  ) 

# TEMP CODE END

# full_pigmentation_TP_Treatment <- full_pigmentation_removed %>%
#   group_by(TP, Treatment) %>%
#   summarise(
#     T5 = mean(T5),
#     T6 = mean(T6),
#     T7 = mean(T7),
#     Sum = mean(Sum)
#   )
# 
# write.csv(full_pigmentation_TP_Treatment, "full_pigmentation_TP_Treatment.csv")
# summary(full_pigmentation_TP_Treatment[3:6])


##########

# 4. Visualization over time

# Sum
p_sum <- full_pigmentation_TP_Treatment %>% 
  ggplot(aes(x=TP, y=Sum, color=Treatment)) +
  geom_line() +
  labs(
    title = "Turgite Sum"
  )

# T5
p_5 <- full_pigmentation_TP_Treatment %>% 
  ggplot(aes(x=TP, y=T5, color=Treatment)) +
  geom_line()  +
  labs(
    title = "Turgite 5"
  )

# T6
p_6 <- full_pigmentation_TP_Treatment %>% 
  ggplot(aes(x=TP, y=T6, color=Treatment)) +
  geom_line()  +
  labs(
    title = "Turgite 6"
  )

# T7
p_7 <- full_pigmentation_TP_Treatment %>% 
  ggplot(aes(x=TP, y=T7, color=Treatment)) +
  geom_line()  +
  labs(
    title = "Turgite 7"
  )


grid.arrange(p_sum, p_5, p_6, p_7)





grid.arrange(p1, p2, p3, p4)


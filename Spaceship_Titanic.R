source("preamble.R", encoding = "UTF8")

# Import des données----
data_train <- read_csv("data/train.csv")
data_test <- read_csv("data/test.csv")

# Retraitement des données----
data <- retraitement_donnees(data_train, data_test)

nrow(data) == length(unique(data$PassengerId)) # L'Id est unique
max(data$IdGroup) == length(unique(data$IdGroup)) # La numérotation des groupes ne comporte aucun trou

summary(data[data$TypeBase == "train",]$IdGroup)
summary(data[data$TypeBase == "test",]$IdGroup)
# La répartition des numéros de groupe semble la même sur les bases d'entraînement et de test

any(data$GroupSize != data$MaxIdNumberInGroup)
# Aucun numéro ne manque au sein des groupes

data |> 
  filter(!is.na(Cabin)) |>
  group_by(Cabin) |>
  summarize(n = n()) |> 
  (\(x) table("Occupants par cabine" = x$n))()
# Les cabines accueillent de 1 à 8 occupants

data |> 
  filter(!is.na(Cabin)) |>
  group_by(Cabin) |>
  summarize(n = length(unique(IdGroup))) |> 
  (\(x) table("Groupes par cabine" = x$n))()
# Les occupants d'une cabine font toujours partie du même groupe
  
data |> 
  filter(!is.na(Cabin)) |>
  group_by(IdGroup) |>
  summarize(n = length(unique(Cabin))) |> 
  (\(x) table("Cabines par groupe" = x$n))()
# Les membres d'un groupe peuvent être répartis dans 1 à 3 cabines

data |> 
  filter(!is.na(Cabin)) |>
  group_by(IdGroup, GroupSize) |>
  summarize(n = length(unique(Cabin))) |> 
  (\(x) table("Taille du groupe" = x$GroupSize, 
              "Nombre de cabines occupées" = x$n))()
# Les membres d'un groupe sont plus susceptibles d'occuper la même cabine

data |> 
  filter(!is.na(Cabin)) |>
  group_by(IdGroup, GroupSize) |>
  summarize(n = length(unique(CabinSide))) |> 
  (\(x) table("Taille du groupe" = x$GroupSize, 
              "Nombre de côtés occupés" = x$n))()
# Les membres d'un groupe ont toujours des cabines du même côté du vaisseau

data |> 
  filter(!is.na(Cabin)) |>
  group_by(IdGroup, CabinInGroup) |>
  summarize(n = length(unique(CabinDeck))) |> 
  (\(x) table("Nombre de cabines occupées" = x$CabinInGroup,
              "Nombre de ponts occupés" = x$n))()
# Lorsque les membres d'un groupe occupent plusieurs cabines, elles sont obligatoirement sur des ponts différents

data |> 
  filter(!is.na(HomePlanet)) |>
  group_by(IdGroup) |>
  summarize(n = length(unique(HomePlanet))) |> 
  (\(x) table("Planètes d'origine par groupe" = x$n))()
# Les membres d'un groupe viennent toujours de la même planète

data |> 
  filter(!is.na(Destination) & !is.na(Cabin)) |>
  group_by(Cabin, CabinSize) |>
  summarize(n = length(unique(Destination))) |> 
  (\(x) table("Occupants de la cabine" = x$CabinSize, 
              "Destinations" = x$n))()
# Les occupants d'une cabine peuvent avoir des destinations différentes

data |> 
  filter(!is.na(CryoSleep) & !is.na(Cabin)) |>
  group_by(Cabin, CabinSize) |>
  summarize(CryoStatus = case_when(
    length(unique(CryoSleep)) == 2 ~ "both",
    CryoSleep ~ "cryo",
    TRUE ~ "not-cryo")) |> 
  (\(x) table("Occupants de la cabine" = x$CabinSize, 
              "Etat de Cryogénisation" = x$CryoStatus))()
# Les occupants d'une cabine peuvent avoir des destinations différentes

data2 <- data |>
  imputation_certaine() |>
  imputation_par_defaut()
data_train2 <- data2 |> filter(PassengerId %in% data_train$PassengerId)
data_test2  <- data2 |> filter(PassengerId %in% data_test$PassengerId)

lkp_hist_b <- function(...) lkp_hist(..., colors = c(lkp_magenta, lkp_green))
lkp_hist_b(data_train, CryoSleep, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, HomePlanet, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, Destination, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, Age, bw = 5, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, VIP, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, CabinArea, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, CabinSide, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
#| fig-asp: 1.2
lkp_hist_b(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinArea, f2 = CabinSide, bw = 1e1,
           fill = Transported, facet_type = "grid", n_breaks_x = 10) +
  theme(legend.position = "bottom")
lkp_density(data_train, TotalExpenses, fill = Transported, from = 0)
lkp_density(data_train |> filter(!NoExpenses), TotalExpenses, fill = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  filter(!NoExpenses) |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  filter(Expenses > 0) |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
lkp_density(data_train |> filter(!NoExpenses), LogTotalExpenses,
            fill = Transported)
data_train |>
  pivot_longer(cols = LogRoomService:LogVRDeck, names_to = "Expense_type", values_to = "LogExpenses") |>
  filter(LogExpenses != 0) |>
  lkp_density(LogExpenses, fill = Expense_type, f1 = Transported, from = 0)
#### CryoSleep----
lkp_hist(data_train, Transported, fill = CryoSleep) +
  scale_fill_manual(values = c(lkp_magenta, lkp_lightblue))
lkp_hist(data_train, HomePlanet, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, Destination, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, Age, bw = 5, fill = CryoSleep)
lkp_hist(data_train, VIP, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, Cabin1, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
source("preamble.R", encoding = "UTF8")
# Import des données----
data_train <- read_csv("data/train.csv")
data_test <- read_csv("data/test.csv")
# Retraitement des données----
data <- bind_rows(data_train, data_test) |>
  retraitement_donnees()
data_train <- data |> filter(PassengerId %in% data_train$PassengerId)
data_test <- data |> filter(PassengerId %in% data_test$PassengerId)
lkp_hist_b <- function(...) lkp_hist(..., colors = c(lkp_magenta, lkp_green))
lkp_hist_b(data_train, CryoSleep, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, HomePlanet, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, Destination, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, Age, bw = 5, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, VIP, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, CabinArea, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist_b(data_train, CabinSide, fill = Transported) +
  theme(legend.position = c(0.9, 0.9))
#| fig-asp: 1.2
lkp_hist_b(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinArea, f2 = CabinSide, bw = 1e1,
           fill = Transported, facet_type = "grid", n_breaks_x = 10) +
  theme(legend.position = "bottom")
lkp_density(data_train, TotalExpenses, fill = Transported, from = 0)
lkp_density(data_train |> filter(!NoExpenses), TotalExpenses, fill = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  filter(!NoExpenses) |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  filter(Expenses > 0) |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
lkp_density(data_train |> filter(!NoExpenses), LogTotalExpenses,
            fill = Transported)
data_train |>
  pivot_longer(cols = LogRoomService:LogVRDeck, names_to = "Expense_type", values_to = "LogExpenses") |>
  filter(LogExpenses != 0) |>
  lkp_density(LogExpenses, fill = Expense_type, f1 = Transported, from = 0)
#### CryoSleep----
lkp_hist(data_train, Transported, fill = CryoSleep) +
  scale_fill_manual(values = c(lkp_magenta, lkp_lightblue))
lkp_hist(data_train, HomePlanet, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, Destination, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, Age, bw = 5, fill = CryoSleep)
lkp_hist(data_train, VIP, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, CabinArea, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train, CabinSide, fill = CryoSleep) +
  theme(legend.position = c(0.9, 0.9))
lkp_hist(data_train |> filter(!is.na(CabinArea)), CabinNumber, f1 = CabinArea, bw = 1e2, fill = CryoSleep)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinSide, bw = 1e2, fill = CryoSleep)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinArea, f2 = CabinSide, bw = 1e2, fill = CryoSleep, facet_type = "grid")
#### HomePlanet----
lkp_hist(data_train, Transported, fill = HomePlanet)
lkp_hist(data_train, CryoSleep, fill = HomePlanet)
lkp_hist(data_train, Destination, fill = HomePlanet)
lkp_hist(data_train, Age, bw = 5, fill = HomePlanet)
lkp_hist(data_train, VIP, fill = HomePlanet)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinArea, f1 = CabinSide, fill = HomePlanet)
lkp_hist(data_train |> filter(!is.na(CabinArea)), CabinNumber, f1 = CabinArea, bw = 1e2, fill = HomePlanet)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinSide, bw = 1e2, fill = HomePlanet)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinArea, f2 = CabinSide, bw = 1e2, fill = HomePlanet, facet_type = "grid")
#### Destination----
lkp_hist(data_train, Transported, fill = Destination)
lkp_hist(data_train, CryoSleep, fill = Destination)
lkp_hist(data_train, HomePlanet, fill = Destination)
lkp_hist(data_train, Age, bw = 5, fill = Destination)
lkp_hist(data_train, VIP, fill = Destination)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinArea, f1 = CabinSide, fill = Destination)
lkp_hist(data_train |> filter(!is.na(CabinArea)), CabinNumber, f1 = CabinArea, bw = 1e2, fill = Destination)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinSide, bw = 1e2, fill = Destination)
lkp_hist(data_train |> filter(!is.na(CabinSide)), CabinNumber, f1 = CabinArea, f2 = CabinSide, bw = 1e2, fill = Destination, facet_type = "grid")
lkp_density(data_train, TotalExpenses, fill = Transported, from = 0)
lkp_density(data_train |> filter(!NoExpenses), TotalExpenses, fill = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  filter(!NoExpenses) |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
data_train |>
  pivot_longer(cols = RoomService:VRDeck, names_to = "Expense_type", values_to = "Expenses") |>
  filter(Expenses > 0) |>
  lkp_density(Expenses, fill = Expense_type, f1 = Transported, from = 0)
lkp_density(data_train |> filter(!NoExpenses), LogTotalExpenses,
            fill = Transported)
data_train |>
  pivot_longer(cols = LogRoomService:LogVRDeck, names_to = "Expense_type", values_to = "LogExpenses") |>
  filter(LogExpenses != 0) |>
  lkp_density(LogExpenses, fill = Expense_type, f1 = Transported, from = 0)
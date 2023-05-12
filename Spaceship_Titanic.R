source("preamble.R", encoding = "UTF8")

# Import des données----
data_train <- read_csv("data/train.csv")
data_test <- read_csv("data/test.csv")

# Retraitement des données----
data <- bind_rows(data_train, data_test) |>
  retraitement_donnees()
data_train <- data |> filter(PassengerId %in% data_train$PassengerId)
data_test <- data |> filter(PassengerId %in% data_test$PassengerId)

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
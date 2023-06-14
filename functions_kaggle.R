complete_expenses <- function(dataset) {

  dataset |>
    mutate(TotalExpenses = RoomService + FoodCourt + ShoppingMall + Spa + VRDeck,
           LogRoomService = if_else(RoomService == 0, - 1, log(RoomService)),
           LogFoodCourt = if_else(FoodCourt == 0, - 1, log(FoodCourt)),
           LogShoppingMall = if_else(ShoppingMall == 0, - 1, log(ShoppingMall)),
           LogSpa = if_else(Spa == 0, - 1, log(Spa)),
           LogVRDeck = if_else(VRDeck == 0, - 1, log(VRDeck)),
           LogTotalExpenses = if_else(TotalExpenses == 0, - 1, log(TotalExpenses)),
           NoRoomService = (RoomService == 0),
           NoFoodCourt = (FoodCourt == 0),
           NoShoppingMall = (ShoppingMall == 0),
           NoSpa = (Spa == 0),
           NoVRDeck = (VRDeck == 0),
           NoExpenses = (TotalExpenses == 0),)
}

retraitement_donnees <- function(data_train, data_test) {
  
  bind_rows(data_train |> mutate(TypeBase = "train"), 
            data_test |> mutate(TypeBase = "test")) |> 
    mutate(IdGroup = word(PassengerId, 1, sep = fixed("_")) |> as.integer(),
           IdNumber = word(PassengerId, 2, sep = fixed("_")) |> as.integer(),
           CabinDeck = word(Cabin, 1, sep = fixed("/")) |> factor(),
           CabinNumber = word(Cabin, 2, sep = fixed("/")) |> as.integer(),
           CabinSide = word(Cabin, 3, sep = fixed("/")) |> factor(),
           FirstName = word(Name, 1) |> factor(),
           LastName = word(Name, 2) |> factor(),
           HomePlanet = HomePlanet |> factor(),
           Destination = Destination |>  factor(),
           TypeBase = TypeBase |> factor()) |> 
    add_count(IdGroup, name = "GroupSize") |>
    add_count(Cabin, name = "CabinSize") |>
    group_by(IdGroup) |> 
    mutate(MaxIdNumberInGroup = max(IdNumber),
           CabinInGroup = length(unique(na.omit(Cabin)))) |> 
    ungroup() |> 
    group_by(CabinDeck, CabinSide) |> 
    mutate(MaxCabinNumberInSector = max(na.omit(CabinNumber)) + 1,
           CabinCountInSector = length(unique(na.omit(Cabin))),
           PersonCountInSector = n()) |> 
    ungroup() |>
    mutate(CabinSize = if_else(is.na(Cabin), NA_integer_, CabinSize),
           MaxCabinNumberInSector = if_else(is.na(Cabin), NA_integer_, MaxCabinNumberInSector)) |> 
    mutate(CabinPosition = CabinNumber / (MaxCabinNumberInSector - 1)) |> 
    complete_expenses()
}

na_replace <- function(v) {
  
  out <- v |> unique()
  if (all(is.na(out))) out else na.omit(v)[[1]]
}
  
imputation_certaine <- function(dataset) {

  dataset |>
    mutate(HomePlanet = case_when(
      is.na(HomePlanet) & !is.na(CabinDeck) & CabinDeck %in% c("A", "B", "C") ~ "Europa",
      is.na(HomePlanet) & !is.na(CabinDeck) & CabinDeck == "G" ~ "Earth",
      TRUE ~ HomePlanet) |> factor(levels = levels(HomePlanet)),
      VIP = if_else(!is.na(HomePlanet) & HomePlanet == "Earth", FALSE, VIP),
      CryoSleep = if_else(!is.na(NoExpenses) & !NoExpenses, FALSE, CryoSleep)) |> 
    mutate(across(RoomService:VRDeck, \(x) if_else(
      (!is.na(Age) & Age < 13) | (!is.na(CryoSleep) & CryoSleep), 0, x))) |> 
    group_by(IdGroup) |> 
    mutate(HomePlanet = na_replace(HomePlanet),
           CabinSide = na_replace(CabinSide)) |> 
    ungroup() |> 
    complete_expenses()
}

most_frequent <- function(v) {
  
  (tibble(x = v) |> count(x))$x[[1]]
}

imputation_par_defaut <- function(dataset) {
  
  dataset |> 
    group_by(HomePlanet, CabinDeck) |> 
    mutate(across(where(is.numeric), \(x) {x[is.na(x)] <- median(x, na.rm = TRUE); x})) |> 
    mutate(across((where(is.logical) | where(is.factor)) & !Transported, \(x) {x[is.na(x)] <- most_frequent(x); x})) |>
    group_by(HomePlanet) |> 
    mutate(across(where(is.numeric), \(x) {x[is.na(x)] <- median(x, na.rm = TRUE); x})) |> 
    mutate(across((where(is.logical) | where(is.factor)) & !Transported, \(x) {x[is.na(x)] <- most_frequent(x); x})) |>
    group_by(CabinDeck) |> 
    mutate(HomePlanet = {HomePlanet[is.na(HomePlanet)] <- most_frequent(HomePlanet); HomePlanet}) |>
    ungroup() |> 
    complete_expenses()
}

submit_model <- function(model_name, dataset) {
  
  model <- get(model_name)
  predictions <- predict(model, dataset, type = "response")
  submission <-
    tibble(PassengerId = dataset$PassengerId,
           Transported = (predictions >= 0.5) |> if_else("True", "False") |> factor())
  write_csv(submission, file = paste0("submissions/", model_name,".csv"))
  
  print(paste0("Model ", model_name, " correctly submitted"))
  return(submission)
}

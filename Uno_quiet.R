#############################################################
# UNO!
# Authors: Regan McCooey and Molly Heffernan
# Date: 5/5/16
# Class: Math 342-04
# Professor: Eric Ruggieri
#############################################################
# create deck makes the deck and returns it as a matrix
#############################################################
create_deck<-function() {
  deck = matrix(data = NA, nrow = 108, ncol = 2, byrow = TRUE)
  c = 1
  for (i in 1:4) {
    if (i == 1) {
      color = "red"
    } else if (i == 2) {
      color = "green"
    } else if (i == 3) {
      color = "yellow"
    } else {
      color = "blue"
    }
    deck[c,] = c(color, 0)
    c = c + 1
    for (num in 1:12) {
      for (j in 1:2) {
        if (num == 10) {
          deck[c,] = c(color, "draw2")
        } else if (num == 11) {
          deck[c,] = c(color, "skip")
        } else if (num == 12) {
          deck[c,] = c(color, "reverse")
        } else {
          deck[c,] = c(color, num)
        }
        c = c + 1
      }
    }
  }
  for (i in 1:4) {
    deck[c,] = c("wild", "reg")
    c = c + 1
    deck[c,] = c("wild", "draw4")
    c = c + 1
  }
  
  perm = sample(1:length(deck[,1]), length(deck[,1]), replace = FALSE)
  deck = deck[perm,]
  return (deck)
}


#############################################################
# deal cards deals 7 cards to the number of players
#############################################################
deal_cards<-function(num_players) {
  
  players = list()
  max_rows = 400
  for (i in 1:num_players) {
    players[[i]] = matrix(data = NA, ncol = 2, nrow = max_rows, byrow = TRUE)
  }
  deck_count = 1
  for (i in 1:7) {
    for (j in 1:num_players) {
      players[[j]][i,] = deck[deck_count,]
      deck_count = deck_count + 1
      
    }
  }
  deck <<- deck[deck_count:length(deck[,1]),]
  return (players)
}


#############################################################
# play game plays the game until one player has a hand 
# count of zero
#############################################################

play_game<-function(num_players) {
  pile <<- matrix(data = NA, ncol = 2, nrow = 200, byrow = TRUE)
  done = FALSE
  colors = c("red", "blue", "yellow", "green")
  status = ""
  #flip over inital card
  #if it's wild - draw again
  pile_count <<- 1
  pile[pile_count,] <<- deck[1,]
  
  deck <<- deck[-1,]
  while (pile[pile_count,1] == "wild") {
    pile_count <<- pile_count + 1
    pile[pile_count,] <<- deck[1,]
    deck <<- deck[-1,]
  }
  
  a = 1
  b = num_players
  
  if (pile[pile_count,2] == "draw2") {
    status = "draw2"
  } else if (pile[pile_count,2] == "draw4") {
    status = "draw4"
  } else if (pile[pile_count,2] == "skip") {
    status = "skip"
  } else if (pile[pile_count,2] == "reverse") {
    b = 1
    a = num_players
  } else {
    status = ""
    a = 1
    b = num_players
  }
  
  
  while (!done) {
    
    count = 0
    
    for (i in a:b) {
      if (length(deck) <= 2) {
        flip_deck(pile)
      }
      if (status == "draw2") {
        if (length(deck[,1]) <= 2) {
          flip_deck(pile)
        }
        for (x in 1:2) {
          player_decks[[i]][player_counts[i]+1,] <<- deck[x,]
          player_counts[i] <<- player_counts[i] + 1
        } 
        deck <<- deck[3:length(deck[,1]),]
        num_picked_up[i] <<- num_picked_up[i] + 2
      } else if (status == "draw4") {
        if (length(deck[,1]) <= 4) {
          flip_deck(pile)
        }
        for (x in 1:4) {
          player_decks[[i]][player_counts[i]+1,] <<- deck[x,]
          player_counts[i] <<- player_counts[i] + 1
        } 
        deck <<- deck[5:length(deck[,1]),]
        num_picked_up[i] <<- num_picked_up[i] + 4
      } else if (status == "skip") {
        if ((i == b) & (a < b)) {
          i = a + 1
        } else if ((i == b) & (a > b)) {
          i = a - 1
        } else if (a < b) {
          i = i + 1
        } else {
          i = i - 1
        }
      }
      move = decide_move(player_decks[[i]], player_counts[i], pile[pile_count,2], pile[pile_count,1], i)
      hand = player_decks[[i]]
      card = hand[move,]
      if (hand[move,1] == "wild") {
        pile[pile_count+1,] <<- card
        pile_count <<- pile_count + 1 
        maxes = color_c(player_decks[[i]])
        index = which.max(maxes)
        card = c(colors[index], "-1")
      } 
      if (hand[move,2] == "draw2") {
        status = "draw2"
      } else if (hand[move,2] == "draw4") {
        status = "draw4"
      } else if (hand[move,2] == "skip") {
        status = "skip"
      } else if (hand[move,2] == "reverse") {
        if (a == 1) {
          b = 1
          a = num_players
        } else {
          a = 1
          b = num_players
        }
        status = ""
      } else {
        status = ""
      }
      pile_count <<- pile_count + 1
      pile[pile_count,] <<- card
      player_decks[[i]] <<- hand[-move,]
      player_counts[i] <<- player_counts[i] - 1
      if (player_counts[i] == 0) {
        done = TRUE
        return(i)
      } 
    }
  }
}

#############################################################
# color_c returns a list of the counts of all of the 
# colors in the player's hand
#############################################################

color_c<-function(colors) {
  num_red = length(colors[(colors == "red") & (!is.na(colors))])
  num_blue = length(colors[(colors == "blue") & (!is.na(colors))])
  num_yellow = length(colors[(colors == "yellow") & (!is.na(colors))])
  num_green = length(colors[(colors == "green") & (!is.na(colors))])
  return(c(num_red, num_blue, num_yellow, num_green))
}

#############################################################
# num_count returns a list of the counts of each number
# in the hand
#############################################################
num_count<-function(numbers) {
  zeros = length(numbers[(numbers == "0") & (!is.na(numbers))])
  ones = length(numbers[(numbers == "1") & (!is.na(numbers))])
  twos = length(numbers[(numbers == "2") & (!is.na(numbers))])
  threes = length(numbers[(numbers == "3") & (!is.na(numbers))])
  fours = length(numbers[(numbers == "4") & (!is.na(numbers))])
  fives = length(numbers[(numbers == "5") & (!is.na(numbers))])
  sixes = length(numbers[(numbers == "6") & (!is.na(numbers))])
  sevens = length(numbers[(numbers == "7") & (!is.na(numbers))])
  eight = length(numbers[(numbers == "8") & (!is.na(numbers))])
  nine = length(numbers[(numbers == "9") & (!is.na(numbers))])
  reverse = length(numbers[(numbers == "reverse") & (!is.na(numbers))])
  skip = length(numbers[(numbers == "skip") & (!is.na(numbers))])
  draw2 = length(numbers[(numbers == "draw2") & (!is.na(numbers))])
  w_draw4 = length(numbers[(numbers == "draw4") & (!is.na(numbers))])
  return(c(zeros, ones, twos, threes, fours, fives, sixes, sevens, eight, nine, 
           reverse, skip, draw2, w_draw4))
}



#############################################################
# this function decides which card the current user 
# should put down on the pile
# Strategy: first it checks if there's a matching number
# then if there is not one it takes the max color 
# and the max number of that color
#############################################################

decide_move<-function(hand, hand_count, curr_num, curr_color, player_num) {
  colors = c("red", "blue", "yellow", "green")
  numbers = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "reverse", "skip", "draw2", "draw4")
  color_count = color_c(hand[1:hand_count,1])
  index = which.max(color_count)
  count = 0
  num_match = check_num(curr_num, hand[,1], hand[,2], colors[index], hand_count)
  
  #find max of one color 
  
  
  has_color = FALSE
  num_of_colors = length(hand[(hand[,1] == curr_color),])
  if (((curr_color == colors[index]) | num_match[1]) & color_count[index] > 0) {
    has_color = TRUE
  }
  color_inhand = FALSE
  #if you don't have the color - don't bother
  if (length(hand[(hand[,1] == curr_color) & !is.na(hand[,1]),]) == 0) {
    color_inhand = FALSE
  } else {
    color_inhand = TRUE
  }
  while (!has_color & count < 3 & color_inhand) {
    color_count = color_count[-index]
    colors = colors[-index]
    index = which.max(color_count)
    count = count + 1
    if (color_count[index] > 0) {
      num_match = check_num(curr_num, hand[,1], hand[,2], colors[index], hand_count)
      if (num_match[1]) {
        has_color = TRUE
      }
    }
    if (curr_color == colors[index]) {
      has_color = TRUE
    }
  }
  
  if (num_match & has_color) {
    return (num_match[2])
  }
  
  count = 0
  if (has_color) {
    nums = num_count(hand[1:hand_count,2])
    index_num = which.max(nums)
    number = numbers[index_num]
    found_num = FALSE
    num_is = 0
    while(!found_num & count <= 13) {
      for (i in 1:hand_count) {
        if ((hand[i,1] == curr_color) & (hand[i,2] == number)) {
          found_num = TRUE
          return(i)
        }
      }
      count = count + 1
      nums = nums[-index_num]
      numbers = numbers[-index_num]
      index_num = which.max(nums)
      number = numbers[index_num]
      num_is = num_is + 1
      if (num_is > hand_count) {
        has_color = FALSE
        break
      }
    }
  }
  
  wilds = hand[hand[1:hand_count,1] == "wild" & !(is.na(hand[1:hand_count,1]))]
  
  if ((!has_color) & (length(wilds) > 0))  {
    for (i in 1:hand_count) {
      if (hand[i,1] == "wild") {
        return (i)
      }
    }
  }
  
  #pick until you have a card that works
  found = FALSE
  num_picked = 1
  
  while((!has_color) & (!found)) {
    if ((length(deck) == 2) || (length(deck[,1]) <= 1)) {
      flip_deck(pile)
    }
    card = deck[num_picked,]
    num_picked = num_picked + 1
    hand[hand_count+1,] = card
    player_decks[[player_num]][hand_count+1,] <<- card
    hand_count = hand_count + 1
    player_counts[player_num] <<- player_counts[player_num] + 1
    num_picked_up[player_num] <<- num_picked_up[player_num] + 1
    if ((length(deck[,1]) - num_picked) <= 0) { #if the deck is almost empty
      flip_deck(pile)
    }
    if ((card[1] == curr_color) | (card[2] == curr_num) | card[1] == "wild") {
      found = TRUE
      deck <<- deck[num_picked:length(deck[,1]),]
      return(hand_count)
    }
    if (is.na(deck[1,])) {
      flip_deck(pile)
    }
  }
}

#############################################################
# check num checks if the hand has the correct number that 
# currently matches the number on the pile
# the index of the number is returned with true
# if not found false is returned with -1
#############################################################

check_num<-function(curr_num, h_color, h_number, color, hand_count) {
  if (curr_num == -1 | color == "wild") {
    return (c(FALSE, -1))
  }
  
  i_returned = -1
  for (i in 1:hand_count) {
    if ((color == h_color[i]) & (curr_num == h_number[i])) {
      i_returned = i
      return (c(TRUE, i_returned))
    }
  }
  return (c(FALSE, -1))
} 

#############################################################
# flip deck makes the pile into the deck if the deck is 
# empty, and then shuffles the deck
#############################################################
flip_deck<-function(pile) {
  curr_color = pile[pile_count, 1]
  curr_num = pile[pile_count, 2]
  new_deck = matrix(data = NA, nrow = 200, ncol = 2, byrow = TRUE)
  start = 0
  if (length(deck) == 2) {
    new_deck[1,] = deck
    start = 1
  } else if (length(deck[,1]) > 0) {
    for (x in 1:(length(deck[,1]))) {
      new_deck[x,] = deck[x,]
      start = x
    }
  }
  
  deck <<- new_deck
  x = pile[(pile[,2] != -1) & (!is.na(pile[,2])),]
  if (length(x) >= 2) {
    for (i in 1:(length(x[,1]))) {
      deck[(i+start),] <<- x[i,]
    }
    perm = sample(1:length(x[,1]), length(x[,1]), replace = FALSE)
    deck <<- deck[perm,]
  }
  
  
  pile <<- matrix(data = NA, nrow = 150, ncol = 2, byrow = TRUE)
  pile[1,1]  <<- curr_color
  pile[1,2] <<- curr_num
  pile_count <<- 1
}

#############################################################
# Count diff colors counts how many different colors you 
# have in the hand 
#############################################################
count_diff_colors<-function(hand) {
  num_colors = c(0, 0, 0, 0)
  for (i in 1:7) {
    if (hand[i,1] == "red") {
      if (num_colors[1] == 0) {
        num_colors[1] = 1
      }
    } else if (hand[i,1] == "blue") {
      if (num_colors[2] == 0) {
        num_colors[2] = 1
      }
    } else if (hand[i,1] == "yellow") {
      if (num_colors[3] == 0) {
        num_colors[3] = 1
      }
    } else if (hand[i,1] == "green") {
      if (num_colors[4] == 0) {
        num_colors[4] = 1
      }
    }
  }
  return(sum(num_colors))
}

#############################################################
# Count different number counts how many different 
# numbers you have in the hand 
#############################################################
count_diff_nums<-function(hand) {
  nums = c(rep(0, 13)) 
  for (i in 1:7) {
    if (hand[i,2] == "0") {
      if (nums[1] == 0) {
        nums[1] = 1
      }
    } else if (hand[i,2] == "1") {
      if (nums[2] == 0) {
        nums[2] = 1
      }
    } else if (hand[i,2] == "2") {
      if (nums[3] == 0) {
        nums[3] = 1
      }
    } else if (hand[i,2] == "3") {
      if (nums[4] == 0) {
        nums[4] = 1
      }
    } else if (hand[i,2] == "4") {
      if (nums[5] == 0) {
        nums[5] = 1
      }
    } else if (hand[i,2] == "5") {
      if (nums[6] == 0) {
        nums[6] = 1
      }
    } else if (hand[i,2] == "6") {
      if (nums[7] == 0) {
        nums[7] = 1
      }
    } else if (hand[i,2] == "7") {
      if (nums[8] == 0) {
        nums[8] = 1
      }
    } else if (hand[i,2] == "8") {
      if (nums[9] == 0) {
        nums[9] = 1
      }
    } else if (hand[i,2] == "9") {
      if (nums[10] == 0) {
        nums[10] = 1
      }
    } else if (hand[i,2] == "draw2") {
      if (nums[11] == 0) {
        nums[11] = 1
      }
    } else if (hand[i,2] == "reverse") {
      if (nums[12] == 0) {
        nums[12] = 1
      }
    } else if (hand[i,2] == "skip") {
      if (nums[13] == 0) {
        nums[13] = 1
      }
    }
  }
  return(sum(nums))
}
#############################################################
# Uno function starts up the game, gets the number of players 
# makes sure it's between 2-5, calls the deal function
# initalizes the hand count array
# then calls play game 
##############################################################
Uno<-function(num_humans) {

  deck <<- create_deck()
  
  if (num_humans > 5 | num_humans <= 1) {
    print("invalid number of players, goodbye")
    return(FALSE)
  }
  deal = deal_cards(num_humans)
  
  player_decks <<- deal
  player_counts <<- NULL
  wilds = NULL
  color_count = NULL
  num_count = NULL
  for (i in 1:num_humans) {
    human = deal[[i]]
    num_wilds = length(human[(human[,1] == "wild") & !(is.na(human[,1]))])
    num_colors = count_diff_colors(human)
    if (num_colors == 4) {
      color_count = c(color_count, 1)
    } else {
      color_count = c(color_count, 0)
    }
    num_nums = count_diff_nums(human)
    if (num_nums >= 7) {
      num_count = c(num_count, 1)
    } else {
      num_count = c(num_count, 0)
    }
    wilds = c(wilds, num_wilds)
    player_counts <<- c(player_counts, 7)
  }
  
  num_picked_up <<- c(rep(0, num_humans))
  player_won = play_game(num_humans)
  return(c(player_won, wilds, color_count, num_count, num_picked_up))
  
}

#probability of winning one game with 3 players

data = matrix(data = NA, nrow = 1000, ncol = 17, byrow = TRUE)
for (i in 1:1000) {
  data[i,] = Uno(4)
}

wild_corr = 0
color_corr = 0
num_corr = 0
pick_corr = 0
p = 4
for (i in 1:1000) {
  if (is.na(data[i,1])) {
    break
  }
  index = data[i,1]
  wilds = data[i,2:(2+(p-1))]
  wild_end = 2+p
  if (which.max(wilds) == index) {
    wild_corr = wild_corr + 1
  }
  col = data[i,wild_end:(wild_end+(p-1))]
  col_end = wild_end+(p-1)+1
  if (which.max(col) == index) {
    color_corr = color_corr + 1
  }
  num = data[i,col_end:(col_end+(p-1))]
  num_end = col_end+(p-1)
  if (which.max(num) == index) {
    num_corr = num_corr + 1
  }
  picked = data[i,(num_end+1):length(data[1,])]
  if (which.min(picked) == index) {
    pick_corr = pick_corr + 1
  }
}

wild_prob = wild_corr/i
color_prob = color_corr/i
num_prob = num_corr/i
pick_prob = pick_corr/i

#different strategies
#wild card first / last
#switch color first if you have number / staying in color 
#memory matrix - always play what you know people don't have 
#always play draw 2/ skip cards first/ last 

#first we should look at probability of winning game in general 
#run 1000 times 


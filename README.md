
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Tracking daily Set completion time

This is a simple shiny app to track daily
[Set](https://www.setgame.com/set/puzzle) completion times among a group
of friends. The data – completion times and players – is stored in an
external database.

### Daily results tab

This tab displays the daily results by each player in `m:ss.sss` format
(1 or more minute digits, 2 second digits, 3 millisecond digits -
optional). “Add/Remove Record” buttons bring up windows in which the
user can add/remove a record for a player on a given date.

### Statistics tab

This tab is a collection of different ways to visualize each player’s
performance. Among others, it displays top 10 times, best time for each
player, cumulative and rolling (7, 30 day) mean of completion times and
mean completion time by day of week.

### Extra functions

For the purposes of this dashboard two complementary functions,
`seconds_to_string()` and `string_to_seconds()`, have been written. The
first one takes in a number of seconds as argument and outputs a string
of `m:ss.sss` format. The other does the opposite – it takes in a string
of `m:ss.sss` format and returns a number of seconds represented by the
string.

# Weaver Codebase Outline

Weaver is made using R and Shiny, unfortunately meaning a lot of the logic is in the `server` function in `app.R`.

## `app.R`
- `regenDBData` function:
    - firstly, estableshes a connection to the database
    - Create a load of tables containing foreign keys and their respective values (i.e. names)
    - We then create `volume_table` using the `loadDBData` function in `db.R`
    - Sets a few default values

- `server` function:
    - **General**
        - This is where the activity of all the Shiny elements gets specified
        - Will (with Loading progress bar) run `regenDBData`. We need to do this on each page load, otherwise it will just keep the data is has for *every* user, and never update unless we restarted Weaver, which isn't ideal.
        - Sets some default values for UI elements
        - Gets any paramenters given in the URL, and updates the filter values
    - **View by Group**
        - `assemblePlot` function:
            - Forms the ggplot object for the "Used Space/Last Modified" graph
            - This is based on whether the user has selected scatter plot or histogram, and what axes they want scaled logarithmically
            - Returns the fully formed plot
        - Few small functions here, such as getting teh size of the selection.
        - This allows us also to generate text saying how much is selected on the graph (in terms of size)
        - Also, the function to clear all the filters is here too
        - `filterTable` function:
            - Takes each of the filters from the left hand side of weaver, and applies them to `filtered_graph_table`, taken from the parameter `table_in`.
            - This returns the the table provided as the parameter (the full data table typically), but filtered according to the filters in the Shiny app
        - The `filtered_table <- eventReactive` calls the `filterTable` function again when any of the filters change. This allows the table to be rerendered
        - `getSelection` is useful, as it will return which of the rows are selected (this also highlights them on the graph, which is the next few `observeEvent`s)
        - `formatTable` allows us to add column titles, sorting and the colours on the status **NOTE: this is where the colours need changing if neccesary**
        - `create_group_detailed_info` function:
            - All of this is done with a "Loading..." progress bar
            - Get the most recently selected PI, Group and Volume, and convert those to their names
            - Using the `getHistory` and `createTrend` functions in `predictions.R`, get all the historical data for that PI/Group/Volume combination, and predictions in 3 days and 7 days.
            - Next, it updates the title and removes the "please select something" warning.
            - Its next job is to plot the graph, using solid lines for historical data, dashed lines for predictions, and different colours for Quota and Usage.
            - Next up, we'll get the warning from the last_selected row. This is the ID number from the DB table
            - Based on the warning given back, we can display appropriate text the user, including telling them where we think their usage will be soon (as a percentage)
            - We're also going to hide any previous tables for user selected predictions - we don't want these hanging around when we select a new record
            - Now, we'll use the `getDirectories` function in `directories.R` to get all the information about subdirectories, and render a table displaying that information
            - We'll also use the `getVaults` function in `vault.R` to get a table about Vault tracked files, and display them here.
            - Finally, we'll make sure the date picker and the download buttons are all available.
        - We've got some `observeEvent`s here just to trigger the `create_group_detailed_info` function when a row is clicked in the main table
        - The final thing we worry about is the date picker, used for creating custom predictions. We use the `createPrediction` function in `predictions.R`, and then render a table to display the information
    - **View by User**
        - The observeEvent is for the submit button (the `isolate`s around the other filters are so that it doesn't update too much)
        - We render a table given by `getUserUsage` (`users.R`) given the three filters, username, group, lustre volume
        - We render a table given by `getVaultHistory` (`vault.R`), given the three filters - filepath, username, lustre volume
    - **Downloads**
        - Each handler here will let us download the table it is associated to, making a `.tsv`
- At the start, we run `regenDBData` to make sure we get some initial data for the first visitor to load
- We then run Shiny, using the UI defined in `ui.R` and the above server function
 
## `db.R`
- `loadDBData` function:
    - First, executes the big SQL query that asks for all the information *for the latest date available for each volume*.
    - We then join up all the foreign key tables
    - Then we change `quota` and `used` columns to doubles
    - We create `quota_use` column which is a percentage of the used space against the quota
    - We then create `used_gib` and `quota_gib` columns, which convert the quota and usage to GiB, as its more readable
    - We also create `is_humgen_yn` and `archived_yn` to change the `1` or `0` to `Yes` or `No`, as its more readable
    - It then returns this table
- `loadScratchDates` function:
    - This asks the DB for the most recent date each volume has data for, which we display to the user, so they know when the data was recorded.
    - `MAX(record_date)` is most recent
    - We also mutate the column to change how the date is formatted
    - The SQL does the formatting here, the data is actually returned from the DB in the form `Volumes: xxx, yyy`

## `predictions.R`
- `collapseInner` and `formatPairs` functions:
    - These two are to format the filters correctly, so we can pump it directly into the SQL query. We have to do it this way, because we want to filter by a pair of columns, which is really simple in SQL, but the R integrations *were not having it* in any form, so we're doing it like this.
- `getHistory` function:
    - This asks the DB for all the data for a group/volume pairing, and also rounds the data after converting it to GiB
- `createPrediction` function:
    - Here, we're giving it a load of history data and a future date, and asking for it to give us the best prediction it can.
    - Obviously, the usual stuff about predictions apply - it can't forsee random things happening - it's just extrapolating the data its got.
    - Firstly, we'll order the history data by date
    - We'll now see how many data points we're going to do the measurement off - ideally 3, but if we've got less data than that, that's how it is.
    - If we've only got one data point, we'll just assume nothing changes - we can't extrapolate a line from a single point!
    - If we can do better, we get some previous data, the difference between the two dates, and calculate the prediction to the future date, and return it
- `createTrend` function:
    - Given some history, we'll order it first, and pull out the quota. We're going to assume the quota doesn't change within the next week.
    - We'll use `createPrediction` to predict the *usage* in 3 days time, and 7 days time.
    - We'll return a dataframe with all this info in.

## `directories.R`
- `getDirectories` function:
    - This is getting the directory information if we can filter with a group and a volume, joined with a concatenation of the information from the filetype table.

## `vault.R`
- `getVaults` function:
    - We're going to get Vault information from the database given a group and volume we can filter by
    - We'll join the `vault_actions` table to get the human-readable names of each action, i.e. Keep, Archive
    - We'll also add on a `size_mib` column, which is the size in MiB.
- `getVaultHistory` function:
    - Here, we can use whichever filters are provided to us (filepath, username, lustre volume, group) to generate the appropriate SQL query to get that data.
    - We have a list of the SQL filters, that we can collapse with AND at the end to generate the query we want.
    - We then ask the DB for that information, join it with the `vault_actions` table, and return it

## `helpers.R`
- `parseBytes` and `readBytes` functions:
    - allow us to convert between bytes and their human readable forms

## `users.R`
- `getUserUsage` function:
    - Here, we'll use whichever filters are provided to us (username, group name, lustre volume) to generate the appropriate SQL query to get that data
    - We have a list of the SQL filters, that we can can collapse together to create the query we want
    - We then ask the DB for that information and return it
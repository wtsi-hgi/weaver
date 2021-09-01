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
        - `assemblePlot` function:
            - Forms the ggplot object for the "Used Space/Last Modified" graph
            - This is based on whether the user has selected scatter plot or histogram, and what axes they want scaled logarithmically
            - Returns the fully formed plot
        - `filterTable` function:
            - Takes each of the filters from the left hand side of weaver, and applies them to `filtered_graph_table`, taken from the parameter `table_in`.
            - This returns the the table provided as the parameter (the full data table typically), but filtered according to the filters in the Shiny app
        - Next up is some reactions to both the `Clear Selection` button and `Clear Filters` button
        - The `filtered_table <- eventReactive` calls the `filterTable` function again when any of the filters change. This allows the table to be rerendered    
    - **Usage Overview Tab**
        - Firstly, we'll render the `assemblePlot`.
        - `getSelection` is useful, as it will return which of the rows are selected (this also highlights them on the graph, which is the next few `observeEvent`s)
    - **Detailed Report Tab**
        - We start by making sure we haven't got any rogue last selected (`ls`) groups or volumes
        - `createHistoryGraph` function:
            - All of this is done with a "Loading..." progress bar
            - Get the most recently selected PI, Group and Volume, and convert those to their names
            - Using the `getHistory` and `createTrend` functions in `predictions.R`, get all the historical data for that PI/Group/Volume combination, and predictions in 3 days and 7 days.
            - Next, it updates the title and removes the "please select something" warning.
            - Its next job is to plot the graph, using solid lines for historical data, dashed lines for predictions, and different colours for Quota and Usage.
            - Next up, we'll call the `calculateWarning` function in `predictions.R`, which'll return either `RED`, `ORANGE` or `GREEN`.
            - Based on the warning given back, we can display appropriate text the user, including telling them where we think their usage will be soon (as a percentage)
            - We're also going to hide any previous tables for user selected predictions - we don't want these hanging around when we select a new record
            - Now, we'll use the `getDirectories` function in `directories.R` to get all the information about subdirectories, and render a table displaying that information
            - We'll also use the `getVaults` function in `vault.R` to get a table about Vault tracked files, and display them here.
            - Finally, we'll make sure the date picker and the sub-tabs are all available.
        - We've got some `observeEvent`s here just to trigger the `createHistoryGraph` function when a row is clicked in either the main table, or a table in the warnings tab.
            - When we get which row it is, for the main table we can just use the `getSelection` function, but not so much for the Warnings tab.
            - Whether you've got "only show non-green statuses" selected completely messes up the numbering here, so we use the `getWarningTable` function in `helpers.R`, passing in the `warnings_no_green` filter to get us the right table
        - We've also got to make sure the information in the Detailed Report is updated if one of the values in the "Other Data" tab is selected. These are all awkward, and have to be fetched with different functions, using different filters :(
            - Firstly, we'll wipe out the History/Predictions bit - we've got no hope getting data for here
            - Render a title
            - We'll now use `getDirectoriesByProject` in `directories.R` and `getVaultsByProject` in `vault.R` to get us the right data, and we can render those tables.
        - The final thing we worry about is the date picker, used for creating custom predictions. We use the `createPrediction` function in `predictions.R`, and then render a table to display the information
    - **Warnings Tab**
        - `decideWarningsSummary` function:
            - This is where we're going to create the table of warnings based on the filters for either PI or lustre volume.
            - First, if no filters have been selected, we'll display a message to the user telling them to select something.
            - We also come up with the appropriate title, based on what filters are applied. It's quite simple: if there's only one filter, is that filter. If there's two, then it's both of them.
            - With a "Loading..." progress bar, we use `formatWarningsTable` in `helpers.R` to give us the table for those filters, using `getSelection`.
            - If there's something to display, we'll display it.
            - If not, we'll (happily) tell them there's no warnings. This typically happens when "only display non-green" is selected, and all the records are green. We like to see this.
        - We also have some `observeEvent`s to recall `decideWarningsSummary` when any of the filters we care about (PI, Volume, No-Green) are changed.
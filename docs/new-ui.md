# New Weaver UI

- [Landing Page](#landing)
- [User Page](#user)
    - [User Filters](#user-filters)
    - [User Tables](#user-tables)
- [Groups Page](#groups)
    - [Warnings](#warnings)
    - [Detailed Reports](#detailed)
- [Other](#other)
- [Help Text](#help)

Currently, although the Weaver UI displays lots of very helpful information to us, it's not always logical about where stuff is, or what is affected when something gets clicked on.

## Landing Page <a name="landing"></a>

As the Weaver UI can easily be split into functions relating to either groups or users, this should be the first choice the user is presented with. ✅

### Options:
- View by User
- View by Group

The landing page would also be a good place to have links to Confluence pages that give more detail about file storage, such as Vault guides. The help text should also either be displayed here, or a popup activated from a help symbol accessable anywhere. ❌

## View by User Page <a name="user"></a>

This will replace the current `User Storage` and `Vault History` tables, so will need to start with the same filters.

### Filters: <a name="user-filters"></a> ✅
- Lustre Volume
- Username
- Unix Group

The Vault History table will also require a `filename` filter. ✅

Below the filters will be the two tables - they can simply be below each other, and not in separate tabs. Unlike currently, they should both follow the same filters. ✅

### Tables: <a name="user-tables"></a>
- User Storage - same columns as current ✅, should be sorted by Size (desc) ✅
- Vault History - same columns as current, and sorted by date as current. ✅ It may be worth adding a note to remind users that using the filename filter will be very helpful here. ✅

**Thought:** If a file is marked as vault->keep, it's just going to stay here, and populate the table each day. Maybe collapsing these if there's a run of consecutive days, saying for example "This has been marked for keeping since 01/08/2021 ❌

The tables should also display dates as `DD/MM/YYYY` instead of `YYYY-MM-DD` (despite that being the superior convention). ✅

It would also be good to do something about the "Submit" button - it needs pressing the first time (pressing Enter key doesn't suffice), but after that - the tables update automatically. ❌

Download buttons could be good here too, just like with the main table in the groups area. ❌

## View by Group Page <a name="groups"></a>

This will replace the current `Modifiers`, `Detailed Report` and `Warnings` tabs.

The filters will be kept as they are, except taken out of the tab, as will the scatter plot. ✅ The plot modifiers can be placed in a row beneath the plot. ✅

Below the plot will be the main table (as it is at the moment). ✅ However, the `Warning Status` column will be moved to the end of this table. ✅ As the `Warnings` tab duplicates a lot of information, and also adds another place to fill the `Detailed Report` tab, this will remove the `Warnings` tab, but not lose any of the information it provides.

We need to make the main table single select only. ✅ Although this means the scatter plot will only display one red dot, this use case is mostly covered by filtering, and it is more important to the user to clearly see which record the detailed information is about. This also means the 'Clear selection' button can be removed. ✅

The 'Download full report' and 'Download table' buttons should work as expected - the download table button shouldn't give addidtional columns purely used for calculation and the download full report button shouldn't download an error message. ❌ Would it maybe be possible to download a PDF displaying **all** the information on the page at that time? ❌

I think it is reasonable to remove the 'Other Data' tab, this was created due to an issue where `cramtastic` wasn't made a HumGen group, and that was needed for testing. The whole 'Other Data' tab basically just shows the anomalies that we should probably hide away. If PIs complain, then we can tell them to manage their data properly. Although it's probably HGI's fault by that point. Oops. ✅

### Warning Levels <a name="warnings"></a>

Currently, Weaver uses a three-tier traffic light warning system:

<table><tr><th></th><th>Today + 3 Days</th><th>Today + 7 Days</th></tr><tr><th>Usage >95%</th><td>Red</td><td>Red</td></tr><tr><th>Usage >90%</th><td>Red</td><td>Orange</td></tr><tr><th>Usage <90%</th><td>Green</td><td>Green</td></tr></table>

There are two issues here:
- Firstly, the threshold for `Orange` is actually very small, and there are hardly any orange warnings, essentially turning this into a two tier system
- The naming isn't very helpful.

To allow sorting, the warning names must be in alphabetical (or reverse alphabetical) order, hence `R -> O -> G`. We're going to change these names:

This is the system we want to have, but we'll need to find something for the middle one that is alphabetically between "OK" and "Not OK". ❌

| Old    | New     |
| :------| :------ |
| Red    | Not OK  |
| Orange |         |
| Green  | OK      |

As many groups are awkwardly close to, or even exceeding, their quota, the 95% rule seems reasonable, but the >90% rule could be recuded to 85% or 80%. It may take some experimenting to set these thresholds. ❌

Also, the equation used to predict this was made before `wrstat` and the increased reliability we get from it, so maybe we need to extrapolate a bit more, rather than taking three datapoints, which now covers three days instead of maybe two weeks. ❌

As the main table isn't sorted by anything, it's perfectly fine to sort it by warning status, with the most extreme at the top. Also, if possible, within each colour banding we should sort by usage percentage descending. ✅

## Detailed Report Information <a name="detailed"></a>

When a record is selected (now only from one table), the information that was in the detailed report section can be updated. This shouldn't be in tabs anymore, and should simply be displayed below the main table, one after the other. ✅

Here is a very simplified version.

```
-------------------------------------------

Main Table

-------------------------------------------

Group Name (PI) - Lustre Volume

-------------------------------------------

History/Future Predictions
--------------------------------
|                              |
|                              |
|                              |
|  The Graph                   |
|                              |
|                              |
|                              |
--------------------------------

Warning Text

----------------
|              |
| Date Picker  |
|              |
----------------

-------------------------------------------

Directory Information
--------------------------------
|                              |
|                              |
|                              |
|  The Directories Table       |
|                              |
|                              |
|                              |
--------------------------------

-------------------------------------------

Vault Information
--------------------------------
|                              |
|                              |
|                              |
|  The Vault Table             |
|                              |
|                              |
|                              |
--------------------------------

-------------------------------------------
```

Also, the warning text displayed below the graph doesn't need to be so 'in your face'. Although it's nice to help colour blind people (and I'd always recommend it), the user doesn't need: 

1. The text being a particular colour
2. The text saying what that colour is
3. An emoji of that colour
4. Another emoji of that colour

Yes, maybe it's good to have the text be red to draw the user's attention to it, but it's not Chernobyl or something - it'll be fine. ✅

The graph that shows the history shows *all* the history it has, which is back to approximately October 2020. This means that, as time goes on, the useful part of the graph gets smaller and smaller. It'd be good to limit how far back the graph goes, I suspect the past **8 months** will suffice. ✅

Once the scale has been changed to a maximum of 8 months, it'd be good to look at whether having the data points on the graph is worthwile. It used to be, as `mpistat` outputs weren't common, however `wrstat` is proving much more reliable, and we're getting so many datapoints, the points basically become the line. ❌

The Vault Information table should also display dates as `DD/MM/YYYY` rather than `YYYY-MM-DD`. ✅

## Other Things <a name="other"></a>

Since we're now dealing with wrstat's improved reliability, although the "Dates Recorded" table is very useful (as it's important to note that the data isn't live), I think it looses its value if every entry is the same every day.

Replacing it with a bar at the top which summarises this would be better. Firstly, now the page is going to be much longer than before, the date should go at the top, opposite the title. Secondly, most of the time it'll be a quite simple display, but when their is inconsistency between dates, that's a lot easier to notice. ✅

**Old Example 1:**

| <b>Dates Data Recorded</b> |            |
| :--------------------------| :----------|
| scratch114                 | 14/09/2021 |
| scratch115                 | 14/09/2021 |
| scratch118                 | 14/09/2021 |
| scratch119                 | 14/09/2021 |
| scratch123                 | 14/09/2021 |

**New Example 1:**

<table><tr><th>Latest Data:<th><td>Volumes 114, 115, 118, 119, 123:</td><td>14/09/2021</td></tr></table>

---

**Old Example 2:**

| <b>Dates Data Recorded</b> |            |
| :--------------------------| :----------|
| scratch114                 | 11/09/2021 |
| scratch115                 | 11/09/2021 |
| scratch118                 | 09/09/2021 |
| scratch119                 | 09/09/2021 |
| scratch123                 | 06/09/2021 |

**New Example 1:**

<table><tr><th>Latest Data:</th><td>Volumes 114, 115:</td><td>11/09/2021</td></tr>
<tr><td></td><td>Volumes 118, 119:</td><td>09/09/2021</td></tr><tr><td></td><td>Volume 123:</td><td>06/09/2021</td></tr></table>

---

The exact way these dates are presented is flexible.

## Help Text <a name="help"></a>
Make sure the help text is up to date once changes are made. ❌

Screenshots may be a good idea. ❌

Is an interactive 'first-time-user' walkthrough worth it/too much work/even possible? ❓
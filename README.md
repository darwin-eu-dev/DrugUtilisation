
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DrugUtilisation <img src="man/figures/logo.png" align="right" height="200"/>

[![CRANstatus](https://www.r-pkg.org/badges/version/DrugUtilisation)](https://CRAN.R-project.org/package=DrugUtilisation)
[![codecov.io](https://codecov.io/github/darwin-eu/DrugUtilisation/coverage.svg?branch=main)](https://app.codecov.io/github/darwin-eu/DrugUtilisation?branch=main)
[![R-CMD-check](https://github.com/darwin-eu/DrugUtilisation/workflows/R-CMD-check/badge.svg)](https://github.com/darwin-eu/DrugUtilisation/actions)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)

## WARNING: This package is under development.

- **addDailyDose** function works for the following patterns in the
  drug_strength table:

<div id="zfhnlcavcp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#zfhnlcavcp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#zfhnlcavcp thead, #zfhnlcavcp tbody, #zfhnlcavcp tfoot, #zfhnlcavcp tr, #zfhnlcavcp td, #zfhnlcavcp th {
  border-style: none;
}
&#10;#zfhnlcavcp p {
  margin: 0;
  padding: 0;
}
&#10;#zfhnlcavcp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#zfhnlcavcp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#zfhnlcavcp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#zfhnlcavcp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#zfhnlcavcp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#zfhnlcavcp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#zfhnlcavcp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#zfhnlcavcp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#zfhnlcavcp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#zfhnlcavcp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#zfhnlcavcp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#zfhnlcavcp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#zfhnlcavcp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#zfhnlcavcp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#zfhnlcavcp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zfhnlcavcp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#zfhnlcavcp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#zfhnlcavcp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#zfhnlcavcp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zfhnlcavcp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#zfhnlcavcp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zfhnlcavcp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#zfhnlcavcp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zfhnlcavcp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#zfhnlcavcp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#zfhnlcavcp .gt_left {
  text-align: left;
}
&#10;#zfhnlcavcp .gt_center {
  text-align: center;
}
&#10;#zfhnlcavcp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#zfhnlcavcp .gt_font_normal {
  font-weight: normal;
}
&#10;#zfhnlcavcp .gt_font_bold {
  font-weight: bold;
}
&#10;#zfhnlcavcp .gt_font_italic {
  font-style: italic;
}
&#10;#zfhnlcavcp .gt_super {
  font-size: 65%;
}
&#10;#zfhnlcavcp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#zfhnlcavcp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#zfhnlcavcp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#zfhnlcavcp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#zfhnlcavcp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#zfhnlcavcp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#zfhnlcavcp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <thead>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="amount">amount</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="amount_unit">amount_unit</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="numerator">numerator</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="numerator_unit">numerator_unit</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="denominator">denominator</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1" style="text-align: center; font-weight: bold;" scope="col" id="denominator_unit">denominator_unit</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" style="background-color: #E9E9E9; font-weight: bold;" scope="colgroup" id="concentration formulation: quantity * numerator / days exposed">concentration formulation: quantity * numerator / days exposed</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">international unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">international unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliequivalent</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Actuation</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">liter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">square centimeter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Actuation</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">square centimeter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">international unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">international unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">mega-international unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliequivalent</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliequivalent</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Actuation</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">liter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">square centimeter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">Actuation</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td></tr>
    <tr><td headers="concentration formulation: quantity * numerator / days exposed  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="concentration formulation: quantity * numerator / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="concentration formulation: quantity * numerator / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">square centimeter</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" style="background-color: #E9E9E9; font-weight: bold;" scope="colgroup" id="fixed amount formulation: quantity * amount / days exposed">fixed amount formulation: quantity * amount / days exposed</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="fixed amount formulation: quantity * amount / days exposed  amount" class="gt_row gt_left" style="text-align: left;">number</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">international unit</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td></tr>
    <tr><td headers="fixed amount formulation: quantity * amount / days exposed  amount" class="gt_row gt_left" style="text-align: left;">number</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">microgram</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td></tr>
    <tr><td headers="fixed amount formulation: quantity * amount / days exposed  amount" class="gt_row gt_left" style="text-align: left;">number</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliequivalent</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td></tr>
    <tr><td headers="fixed amount formulation: quantity * amount / days exposed  amount" class="gt_row gt_left" style="text-align: left;">number</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td></tr>
    <tr><td headers="fixed amount formulation: quantity * amount / days exposed  amount" class="gt_row gt_left" style="text-align: left;">number</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milliliter</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td></tr>
    <tr><td headers="fixed amount formulation: quantity * amount / days exposed  amount" class="gt_row gt_left" style="text-align: left;">number</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="fixed amount formulation: quantity * amount / days exposed  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" style="background-color: #E9E9E9; font-weight: bold;" scope="colgroup" id="time based no denominator: 24 * numerator">time based no denominator: 24 * numerator</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="time based no denominator: 24 * numerator  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="time based no denominator: 24 * numerator  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based no denominator: 24 * numerator  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based no denominator: 24 * numerator  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">microgram</td>
<td headers="time based no denominator: 24 * numerator  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based no denominator: 24 * numerator  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">hour</td></tr>
    <tr><td headers="time based no denominator: 24 * numerator  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="time based no denominator: 24 * numerator  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based no denominator: 24 * numerator  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based no denominator: 24 * numerator  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="time based no denominator: 24 * numerator  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based no denominator: 24 * numerator  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">hour</td></tr>
    <tr class="gt_group_heading_row">
      <th colspan="6" class="gt_group_heading" style="background-color: #E9E9E9; font-weight: bold;" scope="colgroup" id="time based with denominator: if (denominator&amp;gt;24) {numerator * 24 / denominator} else {numerator}">time based with denominator: if (denominator&gt;24) {numerator * 24 / denominator} else {numerator}</th>
    </tr>
    <tr class="gt_row_group_first"><td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">microgram</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">hour</td></tr>
    <tr><td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">milligram</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">hour</td></tr>
    <tr><td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  amount" class="gt_row gt_left" style="text-align: left;">-</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  amount_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">-</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  numerator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  numerator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">unit</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  denominator" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">number</td>
<td headers="time based with denominator: if (denominator>24) {numerator * 24 / denominator} else {numerator}  denominator_unit" class="gt_row gt_left" style="text-align: left; border-left-width: 1px; border-left-style: solid; border-left-color: #D3D3D3; border-right-width: 1px; border-right-style: solid; border-right-color: #D3D3D3; border-top-width: 1px; border-top-style: solid; border-top-color: #D3D3D3; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: #D3D3D3;">hour</td></tr>
  </tbody>
  &#10;  
</table>
</div>

## Package overview

DrugUtilisation contains functions to instantiate and characterize the
cohorts used in a Drug Utilisation Study in the OMOP common data model.
Main functionalities are:

- Create DrugUtilisation cohorts

- Add indications to this cohort

- Add the dosage of a certain ingredient (subseted for a list of drugs)

- Calculate the daily dose

- Create Concept based cohorts

- Read concepts from json files

- Summarise the drug use in a certain cohort

- Summarise the indications in a certain cohort

- Summarise the patients characteristics in a certain cohort

- Summarise the patients large scale characterics in a certain cohort

## Example

First, we need to create a cdm reference for the data we´ll be using.
Here we´ll generate an example with simulated data, but to see how you
would set this up for your database please consult the CDMConnector
package [connection
examples](https://darwin-eu.github.io/CDMConnector/articles/a04_DBI_connection_examples.html).

The package also provides a functionality to generate a
mockDrugUtilisation cdm reference:

``` r
library(DrugUtilisation)
cdm <- mockDrugUtilisation(numberIndividual = 100)
```

### Create a cohort of drug use

To create a cohort we will need a conceptList, this can be read from
json files:

``` r
conceptList <- readConceptList(here::here("Concepts"), cdm)
```

Or we can build our own list using other packages
(e.g. CodelistGenerator)

``` r
conceptList <- getDrugIngredientCodes(cdm, "acetaminophen")
conceptList
#> 
#> ── 1 codelist ──────────────────────────────────────────────────────────────────
#> 
#> - acetaminophen (4 codes)
```

To generate the cohort of drug use we will use
`generateDrugUtilisationCohortSet`:

``` r
cdm <- generateDrugUtilisationCohortSet(
  cdm = cdm,
  name = "dus_cohort",
  conceptSet = conceptList,
  limit = "first",
  priorObservation = 365,
  gapEra = 30,
  priorUseWashout = 0,
  imputeDuration = "none", 
  durationRange = c(0, Inf)
)
#> Warning: The `durationRange` argument of `generateDrugUtilisationCohortSet()` is
#> deprecated as of DrugUtilisation 0.6.2.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `imputeDuration` argument of `generateDrugUtilisationCohortSet()` is
#> deprecated as of DrugUtilisation 0.6.2.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `priorObservation` argument of `generateDrugUtilisationCohortSet()` is
#> deprecated as of DrugUtilisation 0.6.2.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> Warning: The `limit` argument of `generateDrugUtilisationCohortSet()` is deprecated as
#> of DrugUtilisation 0.6.2.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
```

#### Cohort attributes

The generated cohort will have the `GeneratedCohortSet` as seen in
CDMConnector

``` r
class(cdm[["dus_cohort"]])
#> [1] "cohort_table"          "GeneratedCohortSet"    "cdm_table"            
#> [4] "tbl_duckdb_connection" "tbl_dbi"               "tbl_sql"              
#> [7] "tbl_lazy"              "tbl"
```

Cohort set:

``` r
library(CDMConnector)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

``` r
settings(cdm[["dus_cohort"]]) %>% glimpse()
#> Rows: 1
#> Columns: 11
#> $ cohort_definition_id    <int> 1
#> $ cohort_name             <chr> "acetaminophen"
#> $ duration_range_min      <chr> "1"
#> $ duration_range_max      <chr> "Inf"
#> $ impute_duration         <chr> "none"
#> $ gap_era                 <chr> "30"
#> $ prior_use_washout       <chr> "0"
#> $ prior_observation       <chr> "0"
#> $ cohort_date_range_start <chr> NA
#> $ cohort_date_range_end   <chr> NA
#> $ limit                   <chr> "all"
```

Cohort count:

``` r
cohortCount(cdm[["dus_cohort"]])
#> # A tibble: 1 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1             70              62
```

Cohort attrition:

``` r
attrition(cdm[["dus_cohort"]]) %>% glimpse()
#> Rows: 2
#> Columns: 7
#> $ cohort_definition_id <int> 1, 1
#> $ number_records       <int> 71, 70
#> $ number_subjects      <int> 62, 62
#> $ reason_id            <int> 1, 2
#> $ reason               <chr> "Initial qualifying events", "join exposures sepa…
#> $ excluded_records     <int> 0, 1
#> $ excluded_subjects    <int> 0, 0
```

### Indication

Indications will always be cohorts. An option that the package has is to
create concept based cohorts using `generateConceptCohortSet`.

``` r
indications <- list(headache = 378253, influenza = 4266367)
cdm <- generateConceptCohortSet(cdm, indications, "indications_cohort")
cohortCount(cdm[["indications_cohort"]])
#> # A tibble: 2 × 3
#>   cohort_definition_id number_records number_subjects
#>                  <int>          <int>           <int>
#> 1                    1             52              52
#> 2                    2             46              46
```

Then we can add the indication using the function `addIndication`. That
will add a new column for each indication gap and indication.

``` r
x <- cdm[["dus_cohort"]] %>%
  addIndication(
    indicationCohortName = "indications_cohort", indicationGap = c(0, 30, 365), 
    unknownIndicationTable = c("condition_occurrence")
  )
glimpse(x)
#> Rows: ??
#> Columns: 16
#> Database: DuckDB v1.0.0 [nuriamb@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ subject_id                   <int> 88, 79, 9, 98, 48, 57, 77, 40, 63, 47, 35…
#> $ cohort_start_date            <date> 2015-08-19, 2020-05-02, 2016-03-17, 2020…
#> $ cohort_end_date              <date> 2016-05-09, 2022-02-21, 2016-09-02, 2020…
#> $ indication_gap_0_headache    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_influenza   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_none        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_0_unknown     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_influenza  <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_headache   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_none       <dbl> 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_30_unknown    <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_influenza <dbl> 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ indication_gap_365_headache  <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,…
#> $ indication_gap_365_none      <dbl> 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0,…
#> $ indication_gap_365_unknown   <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,…
```

We can combine the indications in a single column using the
`indicationToStrata()` function. This column can be used as
stratification of the results if needed:

``` r
x <- x %>% indicationToStrata(keep = TRUE)
glimpse(x)
#> Rows: ??
#> Columns: 19
#> Database: DuckDB v1.0.0 [nuriamb@Windows 10 x64:R 4.2.3/:memory:]
#> $ cohort_definition_id         <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ subject_id                   <int> 88, 79, 9, 98, 48, 57, 77, 40, 63, 47, 35…
#> $ cohort_start_date            <date> 2015-08-19, 2020-05-02, 2016-03-17, 2020…
#> $ cohort_end_date              <date> 2016-05-09, 2022-02-21, 2016-09-02, 2020…
#> $ indication_gap_0_headache    <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_influenza   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_0_none        <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_0_unknown     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_influenza  <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_headache   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_30_none       <dbl> 1, 1, 1, 0, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1,…
#> $ indication_gap_30_unknown    <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,…
#> $ indication_gap_365_influenza <dbl> 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0,…
#> $ indication_gap_365_headache  <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,…
#> $ indication_gap_365_none      <dbl> 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0,…
#> $ indication_gap_365_unknown   <dbl> 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0,…
#> $ indication_gap_0             <chr> "None", "None", "None", "None", "None", "…
#> $ indication_gap_30            <chr> "None", "None", "None", "Influenza", "Non…
#> $ indication_gap_365           <chr> "Influenza", "None", "None", "Influenza a…
```

``` r
table(x %>% pull("indication_gap_365"))
#> 
#>               Headache              Influenza Influenza and Headache 
#>                      5                      5                      2 
#>                   None                Unknown 
#>                     48                     10
```

### Summarise the indication

We can summarise the indication results using the `summariseIndication`
function:

``` r
summariseIndication(x, cdm)
#> Warning: The `cdm` argument of `summariseIndication()` is deprecated as of
#> DrugUtilisation 0.5.0.
#> This warning is displayed once every 8 hours.
#> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
#> generated.
#> ℹ The following estimates will be computed:
#> • indication_gap_0_headache: count, percentage
#> • indication_gap_0_influenza: count, percentage
#> • indication_gap_0_none: count, percentage
#> • indication_gap_0_unknown: count, percentage
#> • indication_gap_30_influenza: count, percentage
#> • indication_gap_30_headache: count, percentage
#> • indication_gap_30_none: count, percentage
#> • indication_gap_30_unknown: count, percentage
#> • indication_gap_365_influenza: count, percentage
#> • indication_gap_365_headache: count, percentage
#> • indication_gap_365_none: count, percentage
#> • indication_gap_365_unknown: count, percentage
#> • indication_gap_0: count, percentage
#> • indication_gap_30: count, percentage
#> • indication_gap_365: count, percentage
#> → Start summary of data, at 2024-07-02 15:12:33
#> 
#> ✔ Summary finished, at 2024-07-02 15:12:33
#> # A tibble: 46 × 13
#>    result_id cdm_name group_name  group_level   strata_name strata_level
#>        <int> <chr>    <chr>       <chr>         <chr>       <chr>       
#>  1         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  2         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  3         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  4         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  5         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  6         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  7         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  8         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  9         1 DUS MOCK cohort_name acetaminophen overall     overall     
#> 10         1 DUS MOCK cohort_name acetaminophen overall     overall     
#> # ℹ 36 more rows
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
```

``` r
summariseIndication(x, cdm) %>% glimpse()
#> ℹ The following estimates will be computed:
#> • indication_gap_0_headache: count, percentage
#> • indication_gap_0_influenza: count, percentage
#> • indication_gap_0_none: count, percentage
#> • indication_gap_0_unknown: count, percentage
#> • indication_gap_30_influenza: count, percentage
#> • indication_gap_30_headache: count, percentage
#> • indication_gap_30_none: count, percentage
#> • indication_gap_30_unknown: count, percentage
#> • indication_gap_365_influenza: count, percentage
#> • indication_gap_365_headache: count, percentage
#> • indication_gap_365_none: count, percentage
#> • indication_gap_365_unknown: count, percentage
#> • indication_gap_0: count, percentage
#> • indication_gap_30: count, percentage
#> • indication_gap_365: count, percentage
#> → Start summary of data, at 2024-07-02 15:12:33
#> 
#> ✔ Summary finished, at 2024-07-02 15:12:34
#> Rows: 46
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS …
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "acetaminophen", "acetaminophen", "acetaminophen", "a…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "number records", "number subjects", "Indication on i…
#> $ variable_level   <chr> NA, NA, "Headache", "Headache", "Influenza", "Influen…
#> $ estimate_name    <chr> "count", "count", "count", "percentage", "count", "pe…
#> $ estimate_type    <chr> "integer", "integer", "integer", "percentage", "integ…
#> $ estimate_value   <chr> "70", "62", "0", "0", "0", "0", "69", "98.57142857142…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Add strata

All summarise functions have the option to add strata. Strata will
always point to preexisting columns. Here we can see an example where we
create a `age_group` and `sex` columns using PatientProfiles and then we
use it as strata

``` r
x <- x %>%
  PatientProfiles::addAge(ageGroup = list(c(0, 19), c(20, 39), c(40, 59), c(60, 79), c(80, 150))) %>%
  PatientProfiles::addSex()
summariseIndication(x, cdm, strata = list("age_group", "sex", c("age_group", "sex")))
#> ℹ The following estimates will be computed:
#> • indication_gap_0_headache: count, percentage
#> • indication_gap_0_influenza: count, percentage
#> • indication_gap_0_none: count, percentage
#> • indication_gap_0_unknown: count, percentage
#> • indication_gap_30_influenza: count, percentage
#> • indication_gap_30_headache: count, percentage
#> • indication_gap_30_none: count, percentage
#> • indication_gap_30_unknown: count, percentage
#> • indication_gap_365_influenza: count, percentage
#> • indication_gap_365_headache: count, percentage
#> • indication_gap_365_none: count, percentage
#> • indication_gap_365_unknown: count, percentage
#> • indication_gap_0: count, percentage
#> • indication_gap_30: count, percentage
#> • indication_gap_365: count, percentage
#> → Start summary of data, at 2024-07-02 15:12:35
#> 
#> ✔ Summary finished, at 2024-07-02 15:12:35
#> # A tibble: 550 × 13
#>    result_id cdm_name group_name  group_level   strata_name strata_level
#>        <int> <chr>    <chr>       <chr>         <chr>       <chr>       
#>  1         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  2         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  3         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  4         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  5         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  6         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  7         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  8         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  9         1 DUS MOCK cohort_name acetaminophen overall     overall     
#> 10         1 DUS MOCK cohort_name acetaminophen overall     overall     
#> # ℹ 540 more rows
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
```

``` r
summariseIndication(x, cdm, strata = list("age_group", "sex", c("age_group", "sex"))) %>% glimpse()
#> ℹ The following estimates will be computed:
#> • indication_gap_0_headache: count, percentage
#> • indication_gap_0_influenza: count, percentage
#> • indication_gap_0_none: count, percentage
#> • indication_gap_0_unknown: count, percentage
#> • indication_gap_30_influenza: count, percentage
#> • indication_gap_30_headache: count, percentage
#> • indication_gap_30_none: count, percentage
#> • indication_gap_30_unknown: count, percentage
#> • indication_gap_365_influenza: count, percentage
#> • indication_gap_365_headache: count, percentage
#> • indication_gap_365_none: count, percentage
#> • indication_gap_365_unknown: count, percentage
#> • indication_gap_0: count, percentage
#> • indication_gap_30: count, percentage
#> • indication_gap_365: count, percentage
#> → Start summary of data, at 2024-07-02 15:12:36
#> 
#> ✔ Summary finished, at 2024-07-02 15:12:36
#> Rows: 550
#> Columns: 13
#> $ result_id        <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
#> $ cdm_name         <chr> "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS MOCK", "DUS …
#> $ group_name       <chr> "cohort_name", "cohort_name", "cohort_name", "cohort_…
#> $ group_level      <chr> "acetaminophen", "acetaminophen", "acetaminophen", "a…
#> $ strata_name      <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ strata_level     <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ variable_name    <chr> "number records", "number subjects", "Indication on i…
#> $ variable_level   <chr> NA, NA, "Headache", "Headache", "Influenza", "Influen…
#> $ estimate_name    <chr> "count", "count", "count", "percentage", "count", "pe…
#> $ estimate_type    <chr> "integer", "integer", "integer", "percentage", "integ…
#> $ estimate_value   <chr> "70", "62", "0", "0", "0", "0", "69", "98.57142857142…
#> $ additional_name  <chr> "overall", "overall", "overall", "overall", "overall"…
#> $ additional_level <chr> "overall", "overall", "overall", "overall", "overall"…
```

### Daily dose

We can compute daily dose for a certain ingredient from a subset of
drug_exposure or the whole drug exposure (can be very computationally
expensive).

``` r
cdm[["drug_exposure"]] %>%
 addDailyDose(ingredientConceptId = 1125315) %>%
 glimpse()
#> Rows: ??
#> Columns: 9
#> Database: DuckDB v1.0.0 [nuriamb@Windows 10 x64:R 4.2.3/:memory:]
#> $ drug_exposure_id         <int> 8, 12, 17, 23, 24, 27, 34, 35, 40, 49, 50, 59…
#> $ person_id                <int> 3, 4, 7, 8, 9, 9, 12, 13, 14, 19, 19, 23, 25,…
#> $ drug_concept_id          <dbl> 43135274, 1125360, 1125360, 2905077, 1125360,…
#> $ drug_exposure_start_date <date> 1996-09-13, 2004-12-26, 1969-09-09, 2019-09-…
#> $ drug_exposure_end_date   <date> 2008-07-15, 2006-04-15, 1970-02-14, 2020-12-…
#> $ drug_type_concept_id     <dbl> 38000177, 38000177, 38000177, 38000177, 38000…
#> $ quantity                 <dbl> 50, 100, 80, 10, 25, 35, 10, 90, 45, 80, 40, …
#> $ daily_dose               <dbl> 4.625347, 105.042017, 251.572327, 202.105263,…
#> $ unit                     <chr> "milligram", "milligram", "milligram", "milli…
```

#### Coverage

Currently you can evaluate the coverage of daily dose for a conceptList
or overall using `summariseDoseCoverage`. You should restrict to a
certain ingredient.

``` r
summariseDoseCoverage(cdm = cdm, ingredient = 1125315) 
#> ℹ The following estimates will be computed:
#> • daily_dose: count_missing, percentage_missing, mean, sd, min, q05, q25,
#>   median, q75, q95, max
#> ! Table is collected to memory as not all requested estimates are supported on
#>   the database side
#> → Start summary of data, at 2024-07-02 15:12:38
#> 
#> ✔ Summary finished, at 2024-07-02 15:12:39
#> # A tibble: 84 × 13
#>    result_id cdm_name group_name      group_level   strata_name strata_level
#>        <int> <chr>    <chr>           <chr>         <chr>       <chr>       
#>  1         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  2         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  3         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  4         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  5         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  6         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  7         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  8         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#>  9         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#> 10         1 DUS MOCK ingredient_name acetaminophen overall     overall     
#> # ℹ 74 more rows
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
```

### DrugUse

You can add columns related to the drug use using `addDrugUse`. You
always have to provide a reference ingredient.

``` r
x <- x %>%
 addDrugUse(
   ingredientConceptId = 1125315,
   dose = TRUE,
   quantity = TRUE,
   duration = TRUE
 )
```

### Summarise the drug use

You can summarise the drug use using `summariseDrugUse` function

``` r
summariseDrugUse(x)
#> ! names of group will be ignored
#> ℹ The following estimates will be computed:
#> • number_exposures: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> • duration: min, q05, q25, median, q75, q95, max, mean, sd, count_missing,
#>   percentage_missing
#> • cumulative_quantity: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> • number_eras: min, q05, q25, median, q75, q95, max, mean, sd, count_missing,
#>   percentage_missing
#> • initial_quantity: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> • impute_daily_dose_percentage: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> • impute_duration_percentage: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> • initial_daily_dose_milligram: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> • cumulative_dose_milligram: min, q05, q25, median, q75, q95, max, mean, sd,
#>   count_missing, percentage_missing
#> ! Table is collected to memory as not all requested estimates are supported on
#>   the database side
#> → Start summary of data, at 2024-07-02 15:12:46
#> 
#> ✔ Summary finished, at 2024-07-02 15:12:47
#> # A tibble: 101 × 13
#>    result_id cdm_name group_name  group_level   strata_name strata_level
#>        <int> <chr>    <chr>       <chr>         <chr>       <chr>       
#>  1         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  2         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  3         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  4         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  5         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  6         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  7         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  8         1 DUS MOCK cohort_name acetaminophen overall     overall     
#>  9         1 DUS MOCK cohort_name acetaminophen overall     overall     
#> 10         1 DUS MOCK cohort_name acetaminophen overall     overall     
#> # ℹ 91 more rows
#> # ℹ 7 more variables: variable_name <chr>, variable_level <chr>,
#> #   estimate_name <chr>, estimate_type <chr>, estimate_value <chr>,
#> #   additional_name <chr>, additional_level <chr>
```

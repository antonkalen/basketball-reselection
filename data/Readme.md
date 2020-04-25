Data description
================
2020-04-25

The raw data used for this study is composed of 22874 observations
(rows), and 9 variables (columns). Each row is the participation of one
player in a European basketball championship up until the age of 20. A
player can participate in multiple championships the same year.

## Data dictionary

The data contains the following variables:

| Variable             | Variable Name                       | Measurment unit | Allowed values        | Description                                                                   |
| :------------------- | :---------------------------------- | :-------------- | :-------------------- | :---------------------------------------------------------------------------- |
| id                   | Player ID number                    | Integer         | 1-10441               | ID number assigned to each player.                                            |
| birth\_year          | Year of Birth                       | yyyy            | 1988-1997             | The year of birth of the player.                                              |
| birth\_month         | Month of Birth                      | mm              | 1-12                  | The month of birth of the player.                                             |
| country              | Country                             | Text            | Countries             | The national team the player represents.                                      |
| raw\_ranking\_points | Country Ranking Points              | Numeric         | 0-1                   | The gender specific youth national team ranking of the country.               |
| scaled\_log2\_points | Transfoormed Country Ranking Points | Numeric         | \-0.072-0.985         | The log2 transformed and within gender median-centred country ranking points. |
| comp\_year           | Year of Competition                 | yyyy            | 2004-2017             | The year that the competion took place.                                       |
| comp\_category       | Competition Category                | Text            | U16, U18, U20, senior | The age-category of the competition.                                          |

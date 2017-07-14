Feature: Onezone GUI elements
  A user interface for managing Onezone account


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
    And user opened browser window
    And user of browser opened z1 onezone page
    And user of browser entered credentials of user1 in login form in oz login page
    And user of browser clicked on the Sign in button in oz login page


  Scenario: User successfully creates space (presses ENTER after entering text)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser presses enter on keyboard
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully creates space (clicks on confirm button after entering text)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on confirm button displayed next to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User sees that no space has been created after resigning from creating it (clicks cancel button after entering space name)
    When user of browser expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser types "helloworld" to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser clicks on cancel button displayed next to space creation edit box in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel

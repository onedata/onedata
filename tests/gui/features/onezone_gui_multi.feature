Feature: Onezone GUI elements with multiple browsers
  A user interface for managing Onezone account


  Background:
    Given users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened Onezone URL
    And users of [browser1, browser2] clicked on the "plgrid" login button
    And users of [browser1, browser2] logged as [user1, user3]
    And user of browser2 expanded the "go to your files" Onezone sidebar panel
    And user of browser2 clicked on the "p1" provider in Onezone providers sidebar panel
    And user of browser2 clicked on the "Go to your files" button in provider popup
    And user of browser2 seen that Oneprovider session has started
    And user of browser2 clicked on the "spaces" tab in main menu sidebar


  Scenario: User successfully joins space from Onezone gui level (presses ENTER after entering token)
    When user of browser2 clicks on settings icon displayed for "space3" item on the spaces sidebar list
    And user of browser2 clicks on the "INVITE USER" item in settings dropdown for space named "space3"
    And user of browser2 sees that "Invite user to the space" modal has appeared
    And user of browser2 clicks on copy button in active modal
    And user of browser2 sees an info notify with text matching to: .*copied to clipboard.*
    And user of browser2 clicks "OK" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sends copied token to user of browser1

    And user of browser1 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser1 clicks on "Join space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser1 sees that "Join a space" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types received token on keyboard
    And user of browser1 presses enter on keyboard
    And user of browser1 sees that the modal has disappeared

    Then user of browser1 sees that space named "space3" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 selects "space3" from spaces sidebar list
    And user of browser2 refreshes site
    And user of browser2 sees that "user1" item has appeared on current users permissions table

    # TODO remove after integration with swagger
    And user of browser1 expands settings dropdown for space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser1 clicks on the "LEAVE" item in settings dropdown for space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser1 sees that "Leave a space" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that space named "space3" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel


  Scenario: User successfully joins space from Onezone gui level (clicks JOIN confirmation button after entering token)
    When user of browser2 clicks on settings icon displayed for "space3" item on the spaces sidebar list
    And user of browser2 clicks on the "INVITE USER" item in settings dropdown for space named "space3"
    And user of browser2 sees that "Invite user to the space" modal has appeared
    And user of browser2 clicks on copy button in active modal
    And user of browser2 sees an info notify with text matching to: .*copied to clipboard.*
    And user of browser2 clicks "OK" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sends copied token to user of browser1

    And user of browser1 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser1 clicks on "Join space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser1 sees that "Join a space" modal has appeared
    And user of browser1 clicks on input box in active modal
    And user of browser1 types received token on keyboard
    And user of browser1 clicks "Join" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared

    Then user of browser1 sees that space named "space3" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 selects "space3" from spaces sidebar list
    And user of browser2 refreshes site
    And user of browser2 sees that "user1" item has appeared on current users permissions table

    # TODO remove after integration with swagger
    And user of browser1 expands settings dropdown for space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser1 clicks on the "LEAVE" item in settings dropdown for space named "space3" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser1 sees that "Leave a space" modal has appeared
    And user of browser1 clicks "Yes" confirmation button in displayed modal
    And user of browser1 sees that the modal has disappeared
    And user of browser1 sees that space named "space3" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel

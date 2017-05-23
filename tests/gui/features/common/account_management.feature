Feature: Space utilities using onepanel

  Background:
    Given user opened browser window
    And user of browser has account for "z1" Onezone service
    And user of browser opened z1 zone panel page
    And user of browser entered credentials for admin in login form
    And users of browser pressed Sign in button


  Scenario: User successfully changes password
    # change password
    When user of browser clicks on user account button in main menu
    And user of browser clicks on Manage account button in user account popover
    And user of browser clicks on Change password button in account management page
    And user of browser types password for admin in Current password in change password form in account management page
    And user of browser types "heheszki" to New password in change password form in account management page
    And user of browser types "heheszki" to Retype new password in change password form in account management page
    And user of browser clicks on Confirm password change button in change password form in account management page

    # logout
    And user of browser clicks on user account button in main menu
    And user of browser clicks on Logout button in user account popover
    And user of browser sees that he was logged out

    # login with new password
    And user of browser types "admin" to Username input in login form
    And user of browser types "heheszki" to Password input in login form
    And user of browser presses Sign in button
    Then user of browser sees that he successfully logged in zone panel

    # TODO rm after integrating with swagger
    And user of browser clicks on user account button in main menu
    And user of browser clicks on Manage account button in user account popover
    And user of browser clicks on Change password button in account management page
    And user of browser types "heheszki" to Current password in change password form in account management page
    And user of browser types "password" to New password in change password form in account management page
    And user of browser types "password" to Retype new password in change password form in account management page
    And user of browser clicks on Confirm password change button in change password form in account management page

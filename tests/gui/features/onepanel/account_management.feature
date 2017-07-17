Feature: Account management in Onepanel GUI


  Background:
    Given initial users configuration in "z1" Onezone service:
            - admin2:
                password: passwd
                user role: admin

    And user opened browser window
    And user of browser opened z1 zone panel page
    And user of browser logged as admin2 to Onepanel service


  Scenario: User successfully changes password
    # change password
    When user of browser clicks on user account button in main menu
    And user of browser clicks on Manage account button in user account popover
    And user of browser clicks on Change password button in account management page
    And user of browser types password for admin2 in Current password in change password form in account management page
    And user of browser types "heheszki" to New password in change password form in account management page
    And user of browser types "heheszki" to Retype new password in change password form in account management page
    And user of browser clicks on Confirm password change button in change password form in account management page

    # logout
    And user of browser clicks on user account button in main menu
    And user of browser clicks on Logout button in user account popover
    And user of browser sees that he was logged out from Onepanel

    # login with new password
    And user of browser types "admin2" to Username input in Onepanel login form
    And user of browser types "heheszki" to Password input in Onepanel login form
    And user of browser presses Sign in button in Onepanel login page
    Then user of browser sees that he successfully logged in zone panel

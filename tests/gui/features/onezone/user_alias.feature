Feature: Onezone GUI elements featuring alias


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1:
                alias: user1
    And user opened browser window
    And user of browser has account in "z1" Onezone service
    And user of browser opened z1 onezone page
    And user of browser clicked on the "username" login button
    And user of browser seen that "Login with username and password" modal has appeared
    And user of browser entered his credentials in "Login with username and password" modal
    And user of browser clicked "Sign In" confirmation button in displayed modal


  Scenario: User successfully changes his alias (presses ENTER after entering text)
    When user of browser expands the "USER ALIAS" Onezone sidebar panel
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types "alias1" in active edit box
    And user of browser presses enter on keyboard
    Then user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is "alias1"


  Scenario: User sees that his alias remains unchanged after resigning from renaming it (clicks cancel button after entering alias)
    When user of browser expands the "USER ALIAS" Onezone sidebar panel
    And user of browser activates edit box by clicking on the user alias in expanded "USER ALIAS" Onezone panel
    And user of browser types "helloworld" in active edit box
    And user of browser clicks on cancel button displayed next to active edit box
    Then user of browser sees that the user alias displayed in "USER ALIAS" Onezone panel is as recorded one

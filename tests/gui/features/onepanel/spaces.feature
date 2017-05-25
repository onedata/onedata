Feature: Space utilities using onepanel

  Background:
    Given users opened [browser1, browser2, browser3, browser4] browsers' windows
    And users of [browser2, browser3, browser4] have accounts in "z1" Onezone service
    And users of [browser1, browser2] opened [p1 provider panel, z1 onezone] page
    And user of browser1 entered admin credentials in login form
    And users of browser1 pressed Sign in button
    And user of browser2 clicked on the "username" login button
    And user of browser2 seen that "Login with username and password" modal has appeared
    And user of browser2 entered his credentials in "Login with username and password" modal
    And user of browser2 clicked "Sign In" confirmation button in displayed modal


#  Scenario: Support space
#    # create space
#    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser2 sees that there is no space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 clicks on "Create new space" button in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 focuses on activated edit box for creating new space in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 types "helloworld" in active edit box
#    And user of browser2 presses enter on keyboard
#    And user of browser2 sees that space named "helloworld" has appeared in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 refreshes site
#
#    # receive support token
#    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser2 expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
#    And user of browser2 clicks on the "GET SUPPORT" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 sees that dropright with token for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel has appeared
#    And user of browser2 sees that dropright contains non-empty token for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 copy token from dropright for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    And user of browser2 sees an info notify with text matching to: .*copied.*
#    And user of browser2 sees that copied token matches displayed one
#    And user of browser2 sends copied token to user of browser1
#
#    # support space
#    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
#    And user of browser1 clicks on Support space button in spaces page in Onepanel
#    And user of browser1 selects "onestorage" from storage selector in support space form in onepanel
#    And user of browser1 types received token to Support token field in support space form in Onepanel
#    And user of browser1 types "1" to Size input field in support space form in Onepanel
#    And user of browser1 selects GB radio button in support space form in Onepanel
#    And user of browser1 clicks on Support space button in support space form in Onepanel
#    And user of browser1 sees an info notify with text matching to: .*[Aa]dded.*support.*space.*
#    And user of browser1 sees that space support record for "helloworld" has appeared in Spaces page in Onepanel
#
#    # confirm support of space
#    And user of browser2 refreshes site
#    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
#    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
#    Then user of browser2 sees that list of supporting providers for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"


  Scenario: Revoke space support
    Given initial groups configuration in "z1" Onezone service:
        group1:
            owner: browser2
            users:
                - browser3:
                    privileges:
                        - group_invite_user
                        - group_remove_user
                - browser4

    And initial spaces configuration in "z1" Onezone service:
        space1:
            owner: browser2
            users:
                - browser3:
                    privileges:
                      - space_invite_user
                      - space_remove_user
                - browser4
            groups:
                - group1
#            providers:
#                - p1:
#                    storage: /mnt/st1
#                    size: 1000000000

    # assert space existence and support
    When user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 sees that there is space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that list of supporting providers for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel contains only: "p1"

    # unsupport space
    And user of browser1 clicks on Spaces item in submenu of "p1" item in CLUSTERS sidebar in Onepanel
    And user of browser1 clicks on revoke support icon for "helloworld" space support item in Spaces page in Onepanel
    And user of browser1 clicks on Yes, revoke button in Revoke space support popup
    And user of browser1 sees an info notify with text matching to: .*[Ss]upport.*revoked.*

    # confirm lack of support for space
    And user of browser2 refreshes site
    And user of browser2 expands the "DATA SPACE MANAGEMENT" Onezone sidebar panel
    And user of browser2 expands submenu of space named "helloworld" by clicking on space record in expanded "DATA SPACE MANAGEMENT" Onezone panel
    Then user of browser2 sees that there is/are no supporting provider(s) named "p1" for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel

      # TODO remove after integrate with swagger
    And user of browser2 expands settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel by clicking on settings icon
    And user of browser2 clicks on the "LEAVE" item in settings dropdown for space named "helloworld" in expanded "DATA SPACE MANAGEMENT" Onezone panel
    And user of browser2 sees that "Leave a space" modal has appeared
    And user of browser2 clicks "Yes" confirmation button in displayed modal
    And user of browser2 sees that the modal has disappeared
    And user of browser2 sees that space named "helloworld" has disappeared from expanded "DATA SPACE MANAGEMENT" Onezone panel

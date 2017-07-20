Feature: Groups creation in Oneprovider GUI


  Background:
    Given initial users configuration in "z1" Onezone service:
            - user1
            - user2
    And initial groups configuration in "z1" Onezone service:
          group1:
            owner: user1
    And initial spaces configuration in "z1" Onezone service:
          space1:
              owner: user1
              users:
                  - user2
              providers:
                  - p1:
                      storage: onestorage
                      size: 1000000

    And users opened [browser1, browser2] browsers' windows
    And users of [browser1, browser2] opened [z1 onezone, z1 onezone] page
    And users of [browser1, browser2] logged as [user1, user2] to Onezone service
    And users of [browser1, browser2] expanded the "go to your files" Onezone sidebar panel
    And users of [browser1, browser2] clicked on the "p1" provider in Onezone providers sidebar panel
    And users of [browser1, browser2] clicked on the "Go to your files" button in provider popup
    And users of [browser1, browser2] seen that Oneprovider session has started
    And users of [browser1, browser2] clicked on the "groups" tab in main menu sidebar


  Scenario: User fails to view group, to which he does not belong to, using its ID in URL
    When user of browser1 selects "group1" from groups sidebar list
    And user of browser1 copies a first resource ID from URL
    And user of browser1 sends copied group's ID to user of browser2
    And user of browser2 changes webapp path to /#/onedata/groups concatenated with received group's ID
    Then user of browser2 sees an error notify with text matching to: .*?[Cc]annot load requested resource.*?
    And user of browser2 does not see group1 in groups sidebar list

@other_env
Feature: Authorization

  Background:
    Given environment is defined in env.json
    Given environment is up

  Scenario: Successful authorization
#    Given tokens are generated
    Given u1 mounts onedata spaces using token
    Then mounting succeeds
    And s1 are mounted

#  Examples: Successful authorization
#    | token     | spaces |
#    | u1_token  | s1     |

  Scenario: Bad authorization
#    Given tokens are generated
    Given user mounts onedata spaces using bad token
    Then mounting fails

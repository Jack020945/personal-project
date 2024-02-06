test = {
  'name': 'Car',
  'points': 0,
  'suites': [
    {
      'cases': [
        {
          'code': r"""
          >>> from lab07 import *
          >>> srinaths_car = Car('Tesla', 'Model S')
          >>> srinaths_car.model
          'Model S'
          >>> srinaths_car.gas = 10
          >>> srinaths_car.drive()
          'Tesla Model S goes vroom!'
          >>> srinaths_car.drive()
          'Cannot drive!'
          >>> srinaths_car.fill_gas()
          'Gas level: 20'
          >>> srinaths_car.gas
          20
          >>> Car.gas
          30
          """,
          'hidden': False,
          'locked': False
        },
        {
          'code': r"""
          >>> from lab07 import *
          >>> srinaths_car = Car('Tesla', 'Model S')
          >>> srinaths_car.wheels = 2
          >>> srinaths_car.wheels
          2
          >>> Car.num_wheels
          4
          >>> srinaths_car.drive() # Type Error if an error occurs and Nothing if nothing is displayed
          'Cannot drive!'
          >>> Car.drive() # Type Error if an error occurs and Nothing if nothing is displayed
          Error
          >>> Car.drive(srinaths_car) # Type Error if an error occurs and Nothing if nothing is displayed
          'Cannot drive!'
          """,
          'hidden': False,
          'locked': False
        }
      ],
      'scored': False,
      'type': 'wwpp'
    }
  ]
}

class Validation
  attr_reader :error, :valid
  alias_method :valid?, :valid

  def initialize value: '', error: '', valid:
    @value = value
    @error = error
    @valid = valid
  end

  def self.valid value
    Validation.new(value: value, valid: true)
  end

  def self.invalid error
    Validation.new(error: error, valid: false)
  end

  def self.validate validators, input
    validators.reduce(Validation.valid(input)) do |validation, validator|
      fails_here = validation.valid? && !(validator.valid?(input))
      fails_here ? Validation.invalid(validator.error) : validation
    end
  end
end

class Validator
  attr_reader :error

  def initialize check, error
    @check = check
    @error = error
  end

  def valid? input
    @check.call input
  end
end

## End validations -- Start application

validations = [
  Validator.new(->(val) {val != 'foobar'}, 'No foobar allowed.'),
  Validator.new(->(val) {val.length <= 10}, 'Too long.')
]

puts "Hello"
output = Validation.validate validations, gets.chomp
if output.valid?
  puts 'Valid value thanks for playing'
else
  puts "Invalid! #{output.error}"
end

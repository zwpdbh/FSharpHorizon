using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace CSharpDemos
{
    public class Demo05
    {

        public async void Demo()
        {
           await Compute();
        }
        public record Customer(int Id, string? Name);
        public record Data(int Id, int Amount);

        private async Task<Customer?> LoadCustomer(int customerId)
        {
            return customerId == 42 ?
            new(customerId, "Charles") :
            default;
        }

        private async Task<Data?> LoadData(int dataId)
        {
            return new(dataId, 100);
        }

        private string? GetNameOfCustomer(Customer customer)
        {
            return customer.Name;
        }

        public async Task<(string Name, int Amount)?> GetCustomerNameAndAmount(int customerId, int dataId)
        {
            var customer = await LoadCustomer(customerId).ConfigureAwait(false);
            if (customer == null) { return null; }

            var data = await LoadData(dataId).ConfigureAwait(false);
            if (data == null) { return null; }  

            var name = GetNameOfCustomer(customer);
            if (name == null) { return null;}

            return (name, data.Amount);
        }

        public async Task Compute()
        {
            var r = await GetCustomerNameAndAmount(42, 17);
            if (r != null)
            {
                Console.WriteLine($"customer = {r.Value.Name}, amount = {r.Value.Amount}");
            }
            else
            {
                Console.WriteLine("error");
            }
        }
    }


}

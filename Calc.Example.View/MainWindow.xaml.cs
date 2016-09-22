using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace Calc.Example.View
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            DataContext = new ViewModel();
        }

        private void Window_Loaded(object sender, RoutedEventArgs e)
        {
            dataGrid.Columns.Add(new DataGridTextColumn() { Header = "Calculated", IsReadOnly = true, Binding = new Binding("calc") });
            dataGrid.Columns.Add(new DataGridTextColumn() { Header = "number", Binding = new Binding("number") { Mode=BindingMode.TwoWay } });
            dataGrid.Columns.Add(new DataGridTextColumn() { Header = "number 2", Binding = new Binding("number 2") { Mode = BindingMode.TwoWay } });
            dataGrid.Columns.Add(new DataGridTextColumn() { Header = "decimal", Binding = new Binding("decimal") { Mode = BindingMode.TwoWay } });
            dataGrid.Columns.Add(new DataGridTextColumn() { Header = "decimal 2", Binding = new Binding("decimal 2") { Mode = BindingMode.TwoWay } });

            //dataGrid.Columns.Add(new DataGridTextColumn() { Header = "text", Binding = new Binding("text") { Mode = BindingMode.TwoWay } });
            //dataGrid.Columns.Add(new DataGridTextColumn() { Header = "text 2", Binding = new Binding("text 2") { Mode = BindingMode.TwoWay } });
        }
    }
}
